module PPU
  ( State, initState
  , registers, oamDMA
  , ppu
  , Graphics(..)
  ) where

import Control.Monad (when,forM_)
import Data.Array (Array,(!),listArray)
import Data.Bits (testBit,(.&.),(.|.),xor,shiftR)
import Data.Word (Word32)
import Foreign.C.Types (CInt)
import Framework (Eff(..),Ref(..),read,write,update,Bus,dummyRef_quiet)
import Prelude hiding (read)
import SDL (V4(..))
import Text.Printf (printf)
import Types (Addr,U8,Colour,HL(..),makeAddr,splitAddr)
import Data.Map (Map)
import Data.Map qualified as Map

----------------------------------------------------------------------
-- types

-- abstract
type TileId = U8
type MasterColourIndex = U6
type SpriteId = U6
type PaletteId = U5
type ColourIndex = U2
type NameTableId = U2
type PatternTableId = Bool

-- representation
type U6 = U8
type U5 = CInt
type U3 = CInt
type U2 = CInt

----------------------------------------------------------------------
-- ppu

-- In total, we have 262 (1+240+21) lines y:[-1..260]
--   One pre-visible line, y:-1
--   240 visible lines, y:[0..239]
--   21 post-visible lines, y:[240..260]

ppu :: Eff () -> State -> Graphics -> Eff ()
ppu triggerNMI s graphics = loop 0
  where
    State{control,status} = s
    Status{isInVBlankInterval,spriteOverflow,sprite0Hit} = status
    Graphics{displayFrame} = graphics

    loop frame = do
      forM_ [(-1)..261] $ \y -> do
        when (y == -1) preVisibleLine
        when (y >= 0 && y <= 239) $ visibleLine y
        when (y == 241) vblankLine
        AdvancePPU 341
      displayFrame frame
      loop (frame+1)

    preVisibleLine = do
      write False isInVBlankInterval
      write False spriteOverflow
      write False sprite0Hit

    visibleLine y = do
      spritePixMap <- renderScanLineSprites s y
      renderScanLineBG s status graphics y spritePixMap

    vblankLine = do
      write True isInVBlankInterval
      Control{generateNMIOnVBlank=gen} <- read control
      when gen $ triggerNMI

----------------------------------------------------------------------
-- Graphics

data Graphics = Graphics
  { plot :: CInt -> CInt -> Colour -> Eff ()
  , displayFrame :: CInt -> Eff ()
  }

renderScanLineBG :: State -> Status -> Graphics -> CInt -> SpritePixMap -> Eff ()
renderScanLineBG s@State{tab,bus,control} Status{sprite0Hit} Graphics{plot} y spritePixMap = do
  Control{nameTableId, backgroundPatternTableId} <- read control
  let nameTableLocation :: Addr = 0x2000 + fromIntegral (nameTableId * 1024)
  forM_ [0 .. 31] $ \tileX -> do
    let xa = tileX * 8
    let tileY = y `div` 8
    let tileIndex = fromIntegral (tileY * 32 + tileX)
    tileId :: TileId <- bus (nameTableLocation + tileIndex) >>= read
    let tileInsideY = y `mod` 8
    tile <- makeTile s backgroundPatternTableId tileId tileInsideY
    paletteId <- getBackgroundPaletteId s nameTableId xa y
    forM_ [0::U3 ..7] $ \xb -> do
      let x = xa + xb
      let colourIndex = getColourIndex tile xb
      let bgIsOpaque = colourIndex /= 0
      bgCol <- case colourIndex of 0 -> getColour s 0 0
                                   _ -> getColour s paletteId colourIndex
      tab <- read tab
      let optSpritePixel = Map.lookup x spritePixMap
      let
        resolveddCol =
          case optSpritePixel of
            Nothing -> bgCol
            Just (col,Sprite{behindBG}) -> do
              if (tab `xor` behindBG) && bgIsOpaque then bgCol else col
      plot x y resolveddCol

      case optSpritePixel of
        Nothing -> pure ()
        Just (_,Sprite{id}) -> do
          when (id==0 && bgIsOpaque) $ write True sprite0Hit

getBackgroundPaletteId :: State -> NameTableId -> CInt -> CInt -> Eff PaletteId
getBackgroundPaletteId State{bus} nameTableId x y = do
  let xx = x `div` 32
  let yy = y `div` 32
  let nameTableLocation = 0x2000 + fromIntegral (nameTableId * 1024)
  let atableStart = nameTableLocation + 960
  let atableAddr :: Addr = atableStart + fromIntegral (xx + yy * 8)
  atableByte <- bus atableAddr >>= read
  let right = (x `div` 16) `testBit` 0
  let bottom = (y `div` 16) `testBit` 0
  let shift = (if bottom then 4 else 0) + (if right then 2 else 0)
  let twoBits = fromIntegral (atableByte `shiftR` shift) .&. 0x3
  pure twoBits

----------------------------------------------------------------------
-- Sprites

type SpritePixMap = Map CInt (Colour,Sprite)

renderScanLineSprites :: State -> CInt -> Eff SpritePixMap
renderScanLineSprites state y = do
  all <- sequence [ createSprite state id | id::SpriteId <- [0..63] ]
  let visible = [ sprite | sprite <- all, shouldRenderInScanLine sprite y ]
  when (length visible > 8) $ do
    let State {status=Status{spriteOverflow}} = state
    write True spriteOverflow
  -- we render the first 8 sprites which are visible on this scan line
  -- from back to front. so sprites with lower ids are rendered on top of sprites with hight ids
  let spritesToRender = reverse (take 8 visible)
  -- Map.fromList keeps the last value when there are duplicate keys
  Map.fromList . concat <$> sequence [ renderSprite state y sprite | sprite <- spritesToRender ]

data Sprite = Sprite
  { id :: SpriteId
  , spriteX :: CInt
  , spriteY :: CInt
  , is8x16 :: Bool
  , patternTableId :: PatternTableId
  , topTileId :: TileId
  , paletteId :: PaletteId
  , behindBG :: Bool
  , flipX :: Bool
  , flipY :: Bool
  }

createSprite :: State -> SpriteId -> Eff Sprite
createSprite State{control,oamRam} id = do
  Control{sprite8x8PatternTableId,spriteSize=is8x16} <- read control
  let baseAddress :: U8 = 4 * id
  byte0 <- read (oamRam baseAddress)
  byte1 <- read (oamRam (baseAddress + 1))
  byte2 <- read (oamRam (baseAddress + 2))
  byte3 <- read (oamRam (baseAddress + 3))
  let spriteX = fromIntegral byte3
  let spriteY = fromIntegral byte0 + 1
  let patternTableId = if is8x16 then byte1 `testBit` 0 else sprite8x8PatternTableId
  let topTileId = if is8x16 then byte1 .&. 0xfe else byte1
  let paletteId :: PaletteId = 4 + fromIntegral (byte2 .&. 0x3)
  let behindBG = byte2 `testBit` 5
  let flipX = byte2 `testBit` 6
  let flipY = byte2 `testBit` 7
  pure Sprite {id,spriteX,spriteY,is8x16,patternTableId,topTileId
              ,paletteId,behindBG,flipX,flipY}

shouldRenderInScanLine :: Sprite -> CInt -> Bool
shouldRenderInScanLine Sprite{spriteY,is8x16} y = do
  let diffY = y - spriteY
  let height = if is8x16 then 16 else 8
  diffY >= 0 && diffY < height

renderSprite :: State -> CInt -> Sprite -> Eff [(CInt,(Colour,Sprite))]
renderSprite s@State{control} y sprite = do
  Control{spriteSize=is8x16} <- read control
  let Sprite{spriteX,spriteY,patternTableId,topTileId
            ,paletteId,flipX,flipY} = sprite
  let insideY = y - spriteY
  let tileId = topTileId + (if cond then 1 else 0)
        where cond = if flipY && is8x16 then insideY<=7 else insideY>7

  let tileInsideY = insideY `mod` 8
  let tileInsideYflipped = if flipY then 7 - tileInsideY else tileInsideY
  tile <- makeTile s patternTableId tileId tileInsideYflipped
  paletteColour1 <- getColour s paletteId 1
  paletteColour2 <- getColour s paletteId 2
  paletteColour3 <- getColour s paletteId 3

  pure
    [ (x,(col,sprite))
    | (insideX::U3) <- [0..7]
    , let insideXflipped = if flipX then 7-insideX else insideX
    , let colourIndex = getColourIndex tile insideXflipped
    , (colourIndex /= 0)
    , let x = spriteX + insideX
    , let col = case colourIndex of
                       1 -> paletteColour1
                       2 -> paletteColour2
                       3 -> paletteColour3
                       _ -> undefined
    ]

----------------------------------------------------------------------
-- tile

data Tile = TileX
  { loRow :: U8
  , hiRow :: U8
  }

makeTile :: State -> PatternTableId -> TileId -> CInt -> Eff Tile
makeTile State{bus} patternTableId tileId y = do
  let tableAddr = if patternTableId then 0x1000 else 0x0000
  let loPlaneAddr = tableAddr + fromIntegral tileId * 16
  let hiPlaneAddr = loPlaneAddr + 8
  loRow <- bus (loPlaneAddr + fromIntegral y) >>= read
  hiRow <- bus (hiPlaneAddr + fromIntegral y) >>= read
  pure TileX {loRow,hiRow}

----------------------------------------------------------------------
-- Colour

getColourIndex :: Tile -> U3 -> ColourIndex
getColourIndex TileX{loRow,hiRow} x = do
  let i = fromIntegral (7 - x)
  let lo = loRow `testBit` i
  let hi = hiRow `testBit` i
  (if hi then 2 else 0) + (if lo then 1 else 0)

getColour :: State -> PaletteId -> ColourIndex -> Eff Colour
getColour State{bus} paletteId colourIndex = do
  let offset = fromIntegral (paletteId * 4 + colourIndex)
  let a :: Addr = 0x3f00 + offset
  v <- bus a >>= read
  pure $ masterPalette (v `mod` 64)

----------------------------------------------------------------------
-- masterPalette

masterPalette :: MasterColourIndex -> Colour
masterPalette v = do
  arr ! v
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
ppuStatus State{status,latch} = Ref {onRead,onWrite}
  where
    Status{isInVBlankInterval} = status
    onRead = do
      v <- readStatus status
      -- zero two flags after we've read the value
      write False isInVBlankInterval
      write False latch
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
      read (oamRam off)
    onWrite v = do
      off <- read oamOffset
      write v (oamRam off)
      write (off+1) oamOffset

oamDMA :: State -> Bus -> Ref U8
oamDMA State{oamRam,extraCpuCycles} cpuBus = Ref {onRead,onWrite}
  where
    onRead = Error "oamDMA: read"
    onWrite hi = do
      forM_ [0..255] $ \lo -> do
        let a = makeAddr HL {hi,lo}
        v <- cpuBus a >>= read
        write v (oamRam lo)
      update (+513) extraCpuCycles
      pure ()

----------------------------------------------------------------------
-- PPU State

data State = State
  { bus :: Bus
  , control :: Ref Control
  , status :: Status
  , latch :: Ref Bool
  , addr :: Ref Addr
  , buffer :: Ref U8
  , oamOffset :: Ref U8
  , oamRam :: U8 -> Ref U8
  -- tab key: invert sense of "behindBG" when rendering sprite/background. Just for lolz
  , tab :: Ref Bool
  , extraCpuCycles :: Ref Int
  }

initState :: Bus -> Ref Bool -> Ref Int -> Eff State
initState bus tab extraCpuCycles =  do
  control <- DefineRegister (byte2control 0)
  status <- initStatus
  latch <- DefineRegister False
  addr <- DefineRegister 0
  buffer <- DefineRegister 0
  oamOffset <- DefineRegister 0
  oamRamI <- DefineMemory 256
  let oamRam = oamRamI . fromIntegral
  pure State {..}

----------------------------------------------------------------------
-- ControlByte

data Control = Control
  { nameTableId :: NameTableId
  , vramAddressIncrement32 :: Bool
  , sprite8x8PatternTableId  :: Bool
  , backgroundPatternTableId  :: Bool
  , spriteSize :: Bool
  , generateNMIOnVBlank :: Bool
  }

byte2control :: U8 -> Control
byte2control v = Control
  { nameTableId = fromIntegral (v .&. 0x3)
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
