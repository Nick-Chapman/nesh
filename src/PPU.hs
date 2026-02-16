module PPU
  ( State, initState
  , makeRegisters
  , ppu
  , Mode, initMode, nextMode
  , Graphics(..)
  ) where

import Control.Monad (when,forM_)
import Data.Bits (testBit)
import Foreign.C.Types (CInt)
import Framework (Eff(..),Ref(..),read,write,Bus,dummyRef,dummyRef_quiet)
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
normalOperation triggerNMI state graphics = do timing; viewTiles state graphics
  where
    timing = do
      forM_ [(-1)..261] $ \(y::Int) -> do
        when (y == -1) preVisibleLine
        when (y >= 0 && y <= 239) visibleLine
        when (y == 241) vblankLine
        AdvancePPU 341

    preVisibleLine = do
      pure ()

    visibleLine = do
      pure ()

    vblankLine = do
      triggerNMI

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
            let col = lookupPalette testPalette (getColourIndex tile (fromIntegral x))
            forM_ [0..scale-1] $ \yy -> do
              forM_ [0..scale-1] $ \xx -> do
                plot
                  (fromIntegral $ startX + x * scale + xx)
                  (fromIntegral $ startY + y * scale + yy)
                  col
      --AdvancePPU (341 * 260)

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
      | a == 0x2000 -> dummyRef_quiet "ppuCtrl" a
      | a == 0x2001 -> dummyRef_quiet "ppuMask" a
      | a == 0x2002 -> ppuStatus s

      | a == 0x2005 -> dummyRef_quiet "ppuScroll" a
      | a == 0x2006 -> ppuAddr s
      | a == 0x2007 -> ppuData s

      | a == 0x2003 -> dummyRef_quiet "oamAddr" a
      | a == 0x4014 -> dummyRef_quiet "oamDMA" a

      | otherwise ->
        error $ printf "PPU.makeRegisters: address = $%04X" a


ppuStatus :: State -> Ref U8
ppuStatus State{status} = Ref {onRead,onWrite}
  where
    name = "ppuStatus"
    onRead = do
      v <- read status
      --Log $ printf "%s: read -> %02x" name v
      pure v
    onWrite v = do
      Log $ printf "%s: write %02x" name v
      write v status

{-
ppuCtrl :: State -> Ref U8
ppuCtrl State{ctrl} = Ref {onRead,onWrite}
  where
    name = "ppuCtrl"
    onRead = do
      v <- read ctrl
      Log $ printf "%s: read -> %02x" name v
      pure v
    onWrite v = do
      Log $ printf "%s: write %02x" name v
      write v ctrl

ppuMask :: State -> Ref U8
ppuMask State{mask} = Ref {onRead,onWrite}
  where
    name = "ppuMask"
    onRead = do
      v <- read mask
      Log $ printf "%s: read -> %02x" name v
      pure v
    onWrite v = do
      Log $ printf "%s: write %02x" name v
      write v mask

ppuScroll :: State -> Ref U8
ppuScroll State{scroll} = Ref {onRead,onWrite}
  where
    name = "ppuScroll"
    onRead = do
      v <- read scroll
      Log $ printf "%s: read -> %02x" name v
      pure v
    onWrite v = do
      Log $ printf "%s: write %02x" name v
      write v scroll
-}

ppuAddr :: State -> Ref U8
ppuAddr State{addrHI,addrLO,latch} = Ref {onRead,onWrite}
  where
    name = "ppuAddr"
    onRead = Error $ printf "%s: read" name
    onWrite v = do
      latchV <- read latch
      --Log $ printf "%s: write (latch:%s) %02x" name (show latchV) v
      write (not latchV) latch
      case latchV of
        True -> write v addrLO
        False -> write v addrHI

getAddr :: State -> Eff Addr
getAddr State{addrHI,addrLO} = do
  lo <- read addrLO
  hi <- read addrHI
  pure $ makeAddr HL { hi, lo }

incrementAddr :: State -> Eff ()
incrementAddr State{addrHI,addrLO} = do
  let inc = 1 -- TODO: 1 or 32
  lo <- read addrLO
  hi <- read addrHI
  let a = makeAddr HL { hi, lo }
  let HL{hi,lo} = splitAddr (a + inc)
  write hi addrHI
  write lo addrLO

ppuData :: State -> Ref U8
ppuData s@State{bus} = Ref {onRead,onWrite}
  where
    onRead = Error "ppuData/read"
    onWrite v = do
      a <- getAddr s
      --Log $ (printf "ppuData (write) addr (%04x) = %02x" a v)
      bus a >>= write v
      incrementAddr s

----------------------------------------------------------------------
-- PPU Bus

makePpuBus :: Mapper -> Eff Bus -- internal PPU bus containing vmam, pallete ram & chr rom
makePpuBus mapper = do
  vram <- DefineMemory 4096 -- TODO: really only 2K, with mirroring
  pure $ \a -> pure $ do
    if
      | a <= 0x1fff
        -> Mapper.busPPU mapper a

      | a >= 0x2000 && a <= 0x2fff
        -> vram (fromIntegral a - 0x2000)

      | a >= 0x3f00 && a <= 0x3f1f -> dummyRef "pallete RAM" a
      -- TODO: palette ram mirrors

      | otherwise -> do
        error $ printf "PpuBus: unknown address = $%04X" a

----------------------------------------------------------------------
-- PPU State

data State = State -- TODO: rename Context? (because value never changes!)
  { bus :: Bus
  , status :: Ref U8

  , latch :: Ref Bool
  , addrHI :: Ref U8 -- TODO: better to have one register with full 16bit address
  , addrLO :: Ref U8
  }

initState :: Mapper -> Eff State
initState mapper = do
  bus <- makePpuBus mapper
  status <- DefineRegister 0x80 -- ???

  latch <- DefineRegister False
  addrHI <- DefineRegister 0
  addrLO <- DefineRegister 0
  pure State {bus,status,latch,addrHI,addrLO}

----------------------------------------------------------------------
-- Palette

type Colour = RGB

data Palette = Palette (ColourIndex -> Colour)

lookupPalette :: Palette -> ColourIndex -> Colour
lookupPalette (Palette f) = f

testPalette :: Palette
testPalette = Palette $ \case
  I0 -> grey 0x00
  I1 -> grey 0x55
  I2 -> grey 0xAA
  I3 -> grey 0xFF
  where
    grey x = rgb x x x
    rgb r g b = V4 r g b 0

type ColourIndex = U2

data U2 = I0 | I1 | I2 | I3

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
