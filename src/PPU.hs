module PPU
  ( State, initState
  , registers, oamDMA
  , ppu
  , Mode, initMode, nextMode
  , Graphics(..)
  ) where

import Control.Monad (when,forM_)
import Data.Array (Array,(!),listArray)
import Data.Bits (testBit,(.&.),(.|.),shiftR)
import Data.Word (Word32)
import Foreign.C.Types (CInt)
import Framework (Eff(..),Ref(..),read,write,update,Bus,dummyRef_quiet)
import Prelude hiding (read)
import SDL (V4(..))
import Text.Printf (printf)
import Types (Addr,U8,RGB,HL(..),makeAddr,splitAddr)

----------------------------------------------------------------------
-- Graphics

data Mode = Normal | Gradient | ViewTiles
  deriving (Eq,Enum,Bounded,Show)

initMode :: Mode
initMode = Normal

nextMode :: Mode -> Mode
nextMode s = if s == maxBound then minBound else succ s

data Graphics = Graphics
  { plot :: CInt -> CInt -> RGB -> Eff ()
  , displayFrame :: Int -> Eff ()
  }

----------------------------------------------------------------------
-- ppu

ppu :: Eff () -> State -> Graphics -> Eff ()
ppu triggerNMI state@State{mode} graphics = loop 0
  where
    Graphics{displayFrame} = graphics
    loop frame = do
      read mode >>= \case
        Gradient -> do testGradient graphics frame; AdvancePPU (262 * 341)
        ViewTiles -> do viewTiles state graphics; AdvancePPU (262 * 341)
        Normal -> normalOperation triggerNMI state graphics

      displayFrame frame
      loop (frame+1)

----------------------------------------------------------------------
-- shifting gradient test pattern...

testGradient :: Graphics -> Int -> Eff () -- TODO bye
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
-- view-tiles

viewTiles :: State -> Graphics -> Eff () -- TODO bye
viewTiles s Graphics{plot} = oneFrame
  where
    oneFrame = do
      let scale = 2
      let scaledSize = 8 * scale
      let tilesPerRow = 256 `div` scaledSize
      forM_ [0..239] $ \tileId -> do
        let startX = tileId `mod` tilesPerRow * scaledSize
        let startY = tileId `div` tilesPerRow * scaledSize
        forM_ [0..7] $ \y -> do
          tile <- makeTile s False tileId (fromIntegral y)
          forM_ [0..7] $ \x -> do
            let paletteId = 0
            let colourIndex = getColourIndex tile (fromIntegral x)
            col <-
              case colourIndex of
                I0 -> getColour s 0 I0
                _ -> getColour s paletteId colourIndex

            forM_ [0..scale-1] $ \yy -> do
              forM_ [0..scale-1] $ \xx -> do
                plot
                  (fromIntegral $ startX + x * scale + xx)
                  (fromIntegral $ startY + y * scale + yy)
                  col

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
      forM_ [(-1)..261] $ \(y::Int) -> do -- TODO: CInt
        when (y == -1) preVisibleLine
        when (y >= 0 && y <= 239) $ visibleLine y
        when (y == 241) vblankLine
        AdvancePPU 341

    Status{isInVBlankInterval,spriteOverflow} = status

    preVisibleLine = do
      write False isInVBlankInterval
      write False spriteOverflow

    visibleLine y = do
      renderScanLineBG s graphics y
      renderScanLineSprites s graphics (fromIntegral y)

    vblankLine = do
      write True isInVBlankInterval
      Control{generateNMIOnVBlank=gen} <- read control
      when gen $ triggerNMI


renderScanLineBG :: State -> Graphics -> Int -> Eff ()
renderScanLineBG s@State{bus,control} Graphics{plot} y = do
  Control{nameTableId, backgroundPatternTableId} <- read control
  let nameTableLocation :: Addr = 0x2000 + fromIntegral (fromEnum nameTableId * 1024)
  forM_ [0 .. 31] $ \tileX -> do
    let x = tileX * 8
    let tileY = y `div` 8
    let tileIndex = tileY * 32 + tileX
    tileId <- bus (nameTableLocation + fromIntegral tileIndex) >>= read
    let tileInsideY = y `mod` 8
    tile <- makeTile s backgroundPatternTableId (fromIntegral tileId) (fromIntegral tileInsideY)
    paletteId <- getBackgroundPaletteId s nameTableId x y
    forM_ [0::Int ..7] $ \xx -> do
      let colourIndex = getColourIndex tile (fromIntegral xx)
      colour <- case colourIndex of I0 -> getColour s 0 I0
                                    _ -> getColour s paletteId colourIndex
      plot (fromIntegral $ x+xx) (fromIntegral y) colour

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

getBackgroundPaletteId :: State -> U2 -> Int -> Int -> Eff PaletteId
getBackgroundPaletteId State{bus} nameTableId x y = do
  let xx = x `div` 32
  let yy = y `div` 32
  let nameTableLocation = 0x2000 + (fromIntegral.fromEnum) nameTableId * 1024
  let atableStart = nameTableLocation + 960
  let atableAddr :: Addr = atableStart + fromIntegral xx + fromIntegral yy * 8
  atableByte <- bus atableAddr >>= read
  let right :: Bool = (x `div` 16) `testBit` 0
  let bottom = (y `div` 16) `testBit` 0
  let shift = (if bottom then 4 else 0) + (if right then 2 else 0)
  let twoBits = (atableByte `shiftR` shift) .&. 0x3
  pure $ toEnum (fromIntegral twoBits)

----------------------------------------------------------------------
-- tile

type PatternTableId = Bool
type TileId = Int -- TODO U8

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

----------------------------------------------------------------------
-- Colour

getColour :: State -> PaletteId -> ColourIndex -> Eff Colour
getColour State{bus} paletteId colourIndex = do
  let a = 0x3f00 + fromIntegral paletteId * 4 + fromIntegral (fromEnum colourIndex)
  v <- bus a >>= read
  pure $ masterPalette (v `mod` 64)

type PaletteId = Int -- 0..31 ?

type ColourIndex = U2
type Colour = RGB

data U2 = I0 | I1 | I2 | I3 -- TODO: Is this helpful?
  deriving (Eq,Enum)

----------------------------------------------------------------------
-- masterPalette

masterPalette :: U8 -> Colour
masterPalette v = do
  arr ! fromIntegral v -- TODO: minimize need for fromIntegral
  where
    arr :: Array U8 Colour
    arr = listArray (0,63) [ mkCol w | w <- colours ]

    mkCol :: Word32 -> Colour
    mkCol w = V4 r g b i
      where
        r = fromIntegral $ (w `shiftR` 0) .&. 0xff
        g = fromIntegral $ (w `shiftR` 8) .&. 0xff
        b = fromIntegral $ (w `shiftR` 16) .&. 0xff
        i = fromIntegral $ (w `shiftR` 24) .&. 0xff

colours :: [Word32]
colours =
  [0xff626262,0xff902001,0xffa00b24,0xff900047,0xff620060,0xff24006a,0xff001160,0xff002747
  ,0xff003c24,0xff004a01,0xff004f00,0xff244700,0xff623600,0xff000000,0xff000000,0xff000000
  ,0xffababab,0xffe1561f,0xffff394d,0xffef237e,0xffb71ba3,0xff6422b4,0xff0e37ac,0xff00558c
  ,0xff00725e,0xff00882d,0xff009007,0xff478900,0xff9d7300,0xff000000,0xff000000,0xff000000
  ,0xffffffff,0xffffac67,0xffff8d95,0xffff75c8,0xffff6af2,0xffc56fff,0xff6a83ff,0xff1fa0e6
  ,0xff00bfb8,0xff01d885,0xff35e35b,0xff88de45,0xffe3ca49,0xff4e4e4e,0xff000000,0xff000000
  ,0xffffffff,0xffffe0bf,0xffffd3d1,0xffffc9e6,0xffffc3f7,0xffeec4ff,0xffc9cbff,0xffa9d7f7
  ,0xff97e3e6,0xff97eed1,0xffa9f3bf,0xffc9f2b5,0xffeeebb5,0xffb8b8b8,0xff000000,0xff000000
  ]

----------------------------------------------------------------------
-- registers

registers :: State -> Bus
registers s = do
  \a -> pure $ do
    if
      | a == 0x2000 -> ppuCtrl s
      | a == 0x2001 -> dummyRef_quiet "ppuMask" a
      | a == 0x2002 -> ppuStatus s
      | a == 0x2003 -> oamAddr s
      | a == 0x2004 -> oamData s
      | a == 0x2005 -> dummyRef_quiet "ppuScroll" a
      | a == 0x2006 -> ppuAddr s
      | a == 0x2007 -> ppuData s
      | otherwise -> error $ printf "PPU.registers: address = $%04X" a

ppuCtrl :: State -> Ref U8
ppuCtrl State{control} = Ref {onRead,onWrite}
  where
    onRead = Error "ppuCtrl: read"
    onWrite v = write (byte2control v) control

ppuStatus :: State -> Ref U8
ppuStatus State{status} = Ref {onRead,onWrite}
  where
    onRead = do
      --this.isInVBlankInterval = 0 -- TODO
      --this.ppu.registers.ppuAddr.latch = false --TODO
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
ppuData s@State{bus,addr,buffer} = Ref {onRead,onWrite}
  where
    onRead = do
      a <- read addr
      v <- bus a >>= read
      last <- read buffer
      write v buffer
      incrementAddr s
      if | a >= 0x3f00 && a <= 0x3ff -> pure v
         | otherwise -> pure last
    onWrite v = do
      a <- read addr
      --Log $ (printf "ppuData (write) addr (%04x) = %02x" a v)
      bus a >>= write v
      incrementAddr s

incrementAddr :: State -> Eff ()
incrementAddr State{control,addr} = do
  Control{vramAddressIncrement32=inc32} <- read control
  --Log (show ("inc32",inc32)) -- see False and True. Good
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
-- OAM

oamAddr :: State -> Ref U8
oamAddr State{oamOffset} = Ref {onRead,onWrite}
  where
    onRead = Error "oamAddrRegister: read"
    onWrite v = do write v oamOffset

oamData :: State -> Ref U8
oamData State{oamOffset,oamRam} = Ref {onRead,onWrite}
  where
    onRead = do
      off <- read oamOffset
      read (oamRam (fromIntegral off))
    onWrite v = do
      off <- read oamOffset
      write v (oamRam (fromIntegral off))
      write (off+1) oamOffset

oamDMA :: State -> Bus -> Ref U8
oamDMA State{oamRam,extraCpuCycles} cpuBus = Ref {onRead,onWrite}
  where
    onRead = Error "oamDMA: read"
    onWrite hi = do
      forM_ [0..255] $ \lo -> do
        let a = makeAddr HL {hi,lo}
        v <- cpuBus a >>= read
        write v (oamRam (fromIntegral lo))
      update (+513) extraCpuCycles
      pure ()

----------------------------------------------------------------------
-- PPU State

data State = State -- TODO: rename Context? (because value never changes!)
  { bus :: Bus
  , control :: Ref Control
  , status :: Status
  , latch :: Ref Bool
  , addr :: Ref Addr
  , buffer :: Ref U8
  , oamOffset :: Ref U8
  , oamRam :: Int -> Ref U8
  , mode :: Ref Mode
  , extraCpuCycles :: Ref Int
  }

initState :: Bus -> Ref Mode -> Ref Int -> Eff State
initState bus mode extraCpuCycles =  do
  control <- DefineRegister (byte2control 0)
  status <- initStatus
  latch <- DefineRegister False
  addr <- DefineRegister 0
  buffer <- DefineRegister 0
  oamOffset <- DefineRegister 0
  oamRam <- DefineMemory 256
  pure State {bus,control,status,latch,addr,buffer,oamOffset,oamRam,mode
             ,extraCpuCycles
             } -- TODO recordWildcards

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
-- Sprites

renderScanLineSprites :: State -> Graphics -> CInt -> Eff ()
renderScanLineSprites state graphics y = do
  all <- sequence [ createSprite state id | id::Int <- [0..63] ]
  let visible = [ sprite | sprite <- all, shouldRenderInScanLine sprite y ]
  when (length visible > 8) $ do
    let State {status=Status{spriteOverflow}} = state
    write True spriteOverflow
  sequence_ [ renderSprite state graphics y sprite | sprite <- take 8 visible ]

data Sprite = Sprite
  { spriteX :: U8
  , spriteY :: U8
  , is8x16 :: Bool
  , patternTableId :: PatternTableId
  , topTileId :: U8
  , paletteId :: Int
  }

createSprite :: State -> Int -> Eff Sprite
createSprite State{control,oamRam} id = do
  Control{sprite8x8PatternTableId,spriteSize=is8x16} <- read control
  let baseAddress = 4 * id
  byte0 <- read (oamRam baseAddress)
  byte1 <- read (oamRam (baseAddress + 1))
  byte2 <- read (oamRam (baseAddress + 2))
  byte3 <- read (oamRam (baseAddress + 3))
  let spriteX = byte3
  let spriteY = byte0 + 1
  let patternTableId = if is8x16 then byte1 `testBit` 0 else sprite8x8PatternTableId
  let topTileId = if is8x16 then byte1 .&. 0xfe else byte1
  let paletteId = 4 + fromIntegral (byte2 .&. 0x3)
  pure Sprite {spriteX,spriteY,is8x16,patternTableId,topTileId,paletteId}

shouldRenderInScanLine :: Sprite -> CInt -> Bool
shouldRenderInScanLine Sprite{spriteY,is8x16} y = do
  let diffY = y - fromIntegral spriteY
  let height = if is8x16 then 16 else 8
  diffY >= 0 && diffY < height

renderSprite :: State -> Graphics -> CInt -> Sprite -> Eff ()
renderSprite s@State{} Graphics{plot} y sprite = do
  let Sprite{spriteX,spriteY,patternTableId,topTileId,paletteId} = sprite
  let insideY :: CInt = y - fromIntegral spriteY
  let tileId = topTileId + (if insideY > 7 then 1 else 0)
  let tileInsideY :: CInt = insideY `mod` 8
  tile <- makeTile s patternTableId (fromIntegral tileId) (fromIntegral tileInsideY)
  paletteColour1 <- getColour s paletteId I1
  paletteColour2 <- getColour s paletteId I2
  paletteColour3 <- getColour s paletteId I3
  forM_ [0..7] $ \(insideX::CInt) -> do
    let colourIndex = getColourIndex tile (fromIntegral insideX)
    when (colourIndex /= I0) $ do
      let x :: CInt = fromIntegral spriteX + insideX
      let col = case colourIndex of
                     I0 -> error "sprite/colourIndex/0"
                     I1 -> paletteColour1
                     I2 -> paletteColour2
                     I3 -> paletteColour3
      plot x y col
