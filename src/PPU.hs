module PPU
  ( State, initState --, readPosition
  , makeRegisters
  , ppu
  , makePpuBus
  ) where

import CHR qualified (ROM,read)
import Control.Monad (when,forM_)
import Data.Bits (testBit)
import Foreign.C.Types (CInt)
import Framework (Eff(..),Ref(..),read) --,write,update)
import Prelude hiding (read)
import SDL (V4(..))
import Text.Printf (printf)
import Types (Addr,U8,RGB)

data What = Gradient | ViewTiles

ppu :: State -> Eff ()
ppu state = do
  let what = ViewTiles -- select here
  case what of
    Gradient -> testGradient
    ViewTiles ->  viewTiles state

viewTiles :: State -> Eff ()
viewTiles ppuState = loop
  where
    loop = do
      oneFrame
      NewFrame
      AdvancePPU 100_000
      loop

    oneFrame = do
      let scale = 2
      let scaledSize = 8 * scale
      let tilesPerRow = 256 `div` scaledSize
      forM_ [0..239] $ \tileId -> do
        let startX = tileId `mod` tilesPerRow * scaledSize
        let startY = tileId `div` tilesPerRow * scaledSize
        forM_ [0..7] $ \y -> do
          tile <- makeTile ppuState False tileId (fromIntegral y)
          forM_ [0..7] $ \x -> do
            let col = lookupPalette testPalette (getColourIndex tile (fromIntegral x))
            forM_ [0..scale-1] $ \yy -> do
              forM_ [0..scale-1] $ \xx -> do
                Plot
                  (fromIntegral $ startX + x * scale + xx)
                  (fromIntegral $ startY + y * scale + yy)
                  col


testGradient :: Eff ()
testGradient = loop 0
-- In total, we have 262 (1+240+21) lines y:[-1..260]
--   One pre-visible line, y:-1
--   240 visible lines, y:[0..239]
--   21 post-visible lines, y:[240..260]
  where
    loop frame = do
      AdvancePPU 341
      forM_ [0..239] $ \y -> do
        forM_ [0..255] $ \x -> do
          let col = gradientCol frame x y
          Plot x y col
        AdvancePPU 341
      AdvancePPU (21 * 341)
      NewFrame
      loop (frame+1)

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
  let ppuCtrl = makeRegister s "PPUCTRL"
  let ppuStatus = makeRegister s "PPUSTATUS"
  \a -> do
    if
      | a == 0x2000 -> ppuCtrl
      | a == 0x2002 -> ppuStatus

      | otherwise ->
        error $ printf "PPU.makeRegisters: address = $%04X" a

makeRegister :: State -> String -> Ref U8
makeRegister State{} name = Ref {onRead,onWrite}
  where
    log = False
    onRead = do
      let v = 0
      when log $ Log $ printf "%s: read -> %02x" name v
      pure v

    onWrite v = do
      when log $ Log $ printf "%s: write %02x" name v
      pure ()

----------------------------------------------------------------------
-- PPU Bus

type Bus = (Addr -> Ref U8)

makePpuBus :: CHR.ROM -> Eff Bus -- internal PPU bus containing vmam, pallete ram & chr rom
makePpuBus chr = do
  pure $ \a -> do
    if
      | a <= 0x1fff
        -> readCHR chr a

      | otherwise -> do
        error $ printf "makePpuBus: address = $%04X" a


readCHR :: CHR.ROM -> Addr -> Ref U8
readCHR chr a = readonly (CHR.read chr a)
  where
    readonly :: U8 -> Ref U8
    readonly byte =
      Ref { onRead = pure byte
          , onWrite = \v -> error (show ("readonly/onWrite",a,v))
          }

----------------------------------------------------------------------
-- PPU State

data State = State
  { bus :: Addr -> Ref U8
  }

initState :: CHR.ROM -> Eff State
initState chr = do
  bus <- makePpuBus chr
  pure State {bus}

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
  loRow <- read (bus (loPlaneAddr + fromIntegral y))
  hiRow <- read (bus (hiPlaneAddr + fromIntegral y))
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
