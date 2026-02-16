module PPU
  ( State, initState
  , makeRegisters
  , ppu
  , Mode, initMode, nextMode
  , Graphics(..)
  ) where

import Control.Monad (when,forM_)
import Data.Bits (testBit,(.&.),(.|.))
import Foreign.C.Types (CInt)
import Framework (Eff(..),Ref(..),read,write,update,Bus,dummyRef,dummyRef_quiet)
import Mapper (Mapper)
import Mapper qualified (busPPU)
import Prelude hiding (read)
import SDL (V4(..))
import Text.Printf (printf)
import Types (Addr,U8,RGB,HL(..),makeAddr,splitAddr)

----------------------------------------------------------------------
-- Graphics

data Mode = Normal | Gradient1 | ViewTiles
  deriving (Eq,Enum,Bounded,Show)

initMode :: Mode
initMode = Normal

nextMode :: Mode -> Mode
nextMode s = if s == maxBound then minBound else succ s

data Graphics = Graphics
  { plot :: CInt -> CInt -> RGB -> Eff ()
  , displayFrame :: Int -> Eff ()
  , mode :: Ref Mode
  }

----------------------------------------------------------------------
-- ppu

ppu :: Eff () -> State -> Graphics -> Eff ()
ppu triggerNMI state@State{} graphics = loop 0
  where
    Graphics{mode,displayFrame} = graphics
    loop frame = do
      read mode >>= \case
        Normal -> normalOperation triggerNMI state graphics
        Gradient1 -> do testGradient graphics frame; AdvancePPU (262 * 341)
        ViewTiles -> do viewTiles state graphics; AdvancePPU (262 * 341)
      displayFrame frame
      loop (frame+1)

----------------------------------------------------------------------
-- normal operation

-- In total, we have 262 (1+240+21) lines y:[-1..260]
--   One pre-visible line, y:-1
--   240 visible lines, y:[0..239]
--   21 post-visible lines, y:[240..260]

normalOperation :: Eff () -> State -> Graphics -> Eff ()
normalOperation triggerNMI s@State{control,status} graphics = timing
  where

    timing = do
      forM_ [(-1)..261] $ \(y::Int) -> do
        when (y == -1) preVisibleLine
        when (y >= 0 && y <= 239) $ visibleLine y
        when (y == 241) vblankLine
        AdvancePPU 341

    Status{isInVBlankInterval} = status

    preVisibleLine = do
      write False isInVBlankInterval
      pure ()

    visibleLine y = do
      renderScanLine s graphics y
      pure ()

    vblankLine = do
      write True isInVBlankInterval
      Control{generateNMIOnVBlank=gen} <- read control
      when gen $ triggerNMI


renderScanLine :: State -> Graphics -> Int -> Eff ()
renderScanLine s@State{bus,control} Graphics{plot} y = do
  Control{nameTableId, backgroundPatternTableId} <- read control
  let nameTableLocation :: Addr = 0x2000 + fromIntegral (fromEnum nameTableId * 1024)
  forM_ [0 .. 31] $ \tileX -> do
    let x = tileX * 8
    let tileY = y `div` 8
    let tileIndex = tileY * 32 + tileX
    tileId <- bus (nameTableLocation + fromIntegral tileIndex) >>= read
    let tileInsideY = y `mod` 8
    tile <- makeTile s backgroundPatternTableId (fromIntegral tileId) (fromIntegral tileInsideY)
    forM_ [0::Int ..7] $ \xx -> do
      let colourIndex = getColourIndex tile (fromIntegral xx)
      let col = lookupPalette grayPalette colourIndex
      plot (fromIntegral $ x+xx) (fromIntegral y) col

----------------------------------------------------------------------
-- view-tiles

viewTiles :: State -> Graphics -> Eff ()
viewTiles state Graphics{plot} = oneFrame
  where
    oneFrame = do
      let scale = 2
      let scaledSize = 8 * scale
      let tilesPerRow = 256 `div` scaledSize
      forM_ [0..239] $ \tileId -> do
        let startX = tileId `mod` tilesPerRow * scaledSize
        let startY = tileId `div` tilesPerRow * scaledSize
        forM_ [0..7] $ \y -> do
          tile <- makeTile state False tileId (fromIntegral y)
          forM_ [0..7] $ \x -> do
            let col = lookupPalette grayPalette (getColourIndex tile (fromIntegral x))
            forM_ [0..scale-1] $ \yy -> do
              forM_ [0..scale-1] $ \xx -> do
                plot
                  (fromIntegral $ startX + x * scale + xx)
                  (fromIntegral $ startY + y * scale + yy)
                  col

----------------------------------------------------------------------
-- shifting gradient test pattern...

testGradient :: Graphics -> Int -> Eff ()
testGradient Graphics{plot} frame = do
  forM_ [0..239] $ \y -> do
    forM_ [0..255] $ \x -> do
      let col = gradientCol (fromIntegral frame) x y
      plot x y col
  where
    gradientCol :: CInt -> CInt -> CInt -> RGB
    gradientCol frame x y = do
      let r = fromIntegral (y + frame)
      let g = 0
      let b = fromIntegral (x + frame)
      V4 r g b 255

----------------------------------------------------------------------
-- registers

makeRegisters :: State -> Bus
makeRegisters s = do
  \a -> pure $ do
    if
      | a == 0x2000 -> ppuCtrl s
      | a == 0x2001 -> dummyRef_quiet "ppuMask" a
      | a == 0x2002 -> ppuStatus s

      | a == 0x2005 -> dummyRef_quiet "ppuScroll" a
      | a == 0x2006 -> ppuAddr s
      | a == 0x2007 -> ppuData s

      | a == 0x2003 -> dummyRef_quiet "oamAddr" a
      | a == 0x4014 -> dummyRef_quiet "oamDMA" a

      | otherwise ->
        error $ printf "PPU.makeRegisters: address = $%04X" a

ppuCtrl :: State -> Ref U8
ppuCtrl State{control} = Ref {onRead,onWrite}
  where
    onRead = Error "ppuCtrl: read"
    onWrite v = write (byte2control v) control

ppuStatus :: State -> Ref U8
ppuStatus State{status} = Ref {onRead,onWrite}
  where
    onRead = do
      v <- readStatus status
      --Log $ printf "ppuStatus: read -> %02x" v
      pure v
    onWrite v = do
      Error $ printf "ppuStatus: write %02x" v

ppuAddr :: State -> Ref U8
ppuAddr State{addr,latch} = Ref {onRead,onWrite}
  where
    onRead = Error "ppuAddr: read"
    onWrite v = do
      latchV <- read latch
      write (not latchV) latch
      case latchV of
        True -> writeLO v addr
        False -> writeHI v addr

ppuData :: State -> Ref U8
ppuData s@State{bus,addr} = Ref {onRead,onWrite}
  where
    onRead = Error "ppuData/read"
    onWrite v = do
      a <- read addr
      --Log $ (printf "ppuData (write) addr (%04x) = %02x" a v)
      bus a >>= write v
      incrementAddr s

incrementAddr :: State -> Eff ()
incrementAddr State{control,addr} = do
  Control{vramAddressIncrement32=inc32} <- read control
  --Log (show ("inc32",inc32)) -- see False and True
  update (+ (if inc32 then 32 else 1)) addr

writeLO :: U8 -> Ref Addr -> Eff ()
writeLO lo r = do
  a <- read r
  let HL{hi} = splitAddr a
  write (makeAddr HL {hi,lo}) r

writeHI :: U8 -> Ref Addr -> Eff ()
writeHI hi r = do
  a <- read r
  let HL{lo} = splitAddr a
  write (makeAddr HL {hi,lo}) r

----------------------------------------------------------------------
-- PPU Bus

makePpuBus :: Mapper -> Eff Bus -- internal PPU bus containing vmam, pallete ram & chr rom
makePpuBus mapper = do
  vram <- DefineMemory 4096 -- TODO: really only 2K, with mirroring

  let paletteRam = dummyRef "palete ram" -- TODO

  pure $ \a -> pure $ do
    if
      | a <= 0x1fff
        -> Mapper.busPPU mapper a

      -- vram
      | a >= 0x2000 && a <= 0x2fff
        -> vram (fromIntegral a - 0x2000)

      {- vram mirror -- TODO
      | a >= 0x3000 && a <= 0x3eff
        -> vram (fromIntegral a - 0x3000) -}

      -- palette ram
      | a >= 0x3f00 && a <= 0x3f1f -> paletteRam (a - 0x3f00)

      {- palette ram mirrors -- TODO
      | a >= 0x3f20 && a <= 0x3fff
        -> paletteRam ((a - 0x3f20) `mod` 0x20) -}

      | otherwise -> do
        error $ printf "PpuBus: unknown address = $%04X" a

----------------------------------------------------------------------
-- PPU State

data State = State -- TODO: rename Context? (because value never changes!)
  { bus :: Bus
  , control :: Ref Control
  , status :: Status

  , latch :: Ref Bool
  , addr :: Ref Addr
  }

initState :: Mapper -> Eff State
initState mapper = do
  bus <- makePpuBus mapper
  control <- DefineRegister (byte2control 0)
  status <- initStatus
  latch <- DefineRegister False
  addr <- DefineRegister 0
  pure State {bus,control,status,latch,addr}

----------------------------------------------------------------------
-- ControlByte

data Control = Control
  { nameTableId :: U2
  , vramAddressIncrement32 :: Bool
  , sprite8x8PatternTableId  :: Bool
  , backgroundPatternTableId  :: Bool
  , spriteSize :: Bool
  , generateNMIOnVBlank :: Bool
  }

byte2control :: U8 -> Control
byte2control v = Control
  { nameTableId = toEnum (fromIntegral (v .&. 0x3))
  , vramAddressIncrement32 = v `testBit` 2
  , sprite8x8PatternTableId = v `testBit` 3
  , backgroundPatternTableId = v `testBit` 4
  , spriteSize = v `testBit` 5
  -- Nothing in bit position 6
  , generateNMIOnVBlank = v `testBit` 7
  }

----------------------------------------------------------------------
-- StatusByte

data Status = Status
  { spriteOverflow :: Ref Bool
  , sprite0Hit :: Ref Bool
  , isInVBlankInterval :: Ref Bool
  }

initStatus :: Eff Status
initStatus = do
  spriteOverflow <- DefineRegister False
  sprite0Hit <- DefineRegister False
  isInVBlankInterval <- DefineRegister True
  pure Status { spriteOverflow, sprite0Hit, isInVBlankInterval }

readStatus :: Status -> Eff U8
readStatus Status{spriteOverflow,sprite0Hit,isInVBlankInterval} = do
  spriteOverflow <- read spriteOverflow
  sprite0Hit <- read sprite0Hit
  isInVBlankInterval <- read isInVBlankInterval
  pure $
    (if spriteOverflow then 0x20 else 0) .|.
    (if sprite0Hit then 0x40 else 0) .|.
    (if isInVBlankInterval then 0x80 else 0)

----------------------------------------------------------------------
-- Palette

type Colour = RGB

data Palette = Palette (ColourIndex -> Colour)

lookupPalette :: Palette -> ColourIndex -> Colour
lookupPalette (Palette f) = f

grayPalette :: Palette
grayPalette = Palette $ \case
  I0 -> grey 0x00
  I1 -> grey 0x55
  I2 -> grey 0xAA
  I3 -> grey 0xFF
  where
    grey x = rgb x x x
    rgb r g b = V4 r g b 0

type ColourIndex = U2

data U2 = I0 | I1 | I2 | I3
  deriving (Enum)

----------------------------------------------------------------------
-- Tile

type PatternTableId = Bool
type TileId = Int

data Tile = TileX
  { loRow :: U8
  , hiRow :: U8
  }

makeTile :: State -> PatternTableId -> TileId -> U8 -> Eff Tile
makeTile State{bus} patternTableId tileId y = do
  let tableAddr = if patternTableId then 0x1000 else 0x0000
  let loPlaneAddr = tableAddr + fromIntegral tileId * 16
  let hiPlaneAddr = loPlaneAddr + 8
  loRow <- bus (loPlaneAddr + fromIntegral y) >>= read
  hiRow <- bus (hiPlaneAddr + fromIntegral y) >>= read
  pure TileX {loRow,hiRow}

getColourIndex :: Tile -> U8 -> ColourIndex
getColourIndex TileX{loRow,hiRow} x = do
  let i = 7 - fromIntegral x
  let lo = loRow `testBit` i
  let hi = hiRow `testBit` i
  case (hi,lo) of
    (False,False) -> I0
    (False,True) -> I1
    (True,False) -> I2
    (True,True) -> I3
