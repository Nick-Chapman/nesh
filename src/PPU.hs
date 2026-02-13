module PPU
  ( State, initState, readPosition
  , makeRegisters
  , ppu
  ) where

import Control.Monad (when,forM_)
import Framework --(Eff(..),Ref(..),read,write,update)
import Prelude hiding (read)
import Text.Printf (printf)
import Types (Addr,U8,RGB) --(..))
import Foreign.C.Types (CInt)
import SDL (V4(..))

ppu :: State -> Eff ()
ppu _s = loop 0
  where

    loop :: CInt -> Eff ()
    loop frame = do
      --AdvancePPU 1
      --_tickPixel s
      --_render s

      -- In total, we have 262 (1+240+21) lines y:[-1..260]

      AdvancePPU 341 -- one pre-visible line (y= -1)
      forM_ [0..239] $ \y -> do -- 240 visible lines, y:[0..239]
        forM_ [0..255] $ \x -> do
          let col = gradientCol frame x y
          Plot x y col
        AdvancePPU 341
      AdvancePPU (21 * 341) -- 21 post-visible lines, y:[240..260]
      --AdvancePPU (262 * 341)
      NewFrame
      loop (frame+1)

gradientCol :: CInt -> CInt -> CInt -> RGB
gradientCol frame x y = do
  let r = fromIntegral (y + frame)
  let g = 0
  let b = fromIntegral (x + frame)
  V4 r g b 255


--visible :: CInt -> CInt -> Bool
--visible x y = (x >= 0 && x < 256) && (y >= 0 && y < 240)

{-
tickPixel :: State -> Eff ()
tickPixel State{frame,y,x} = do
  update (+1) x
  xv <- read x
  when (xv == 341) $ do
    write x 0
    update (+1) y
    yv <- read y
    when (yv == 261) $ do
      write y (-1)
      update (+1) frame
      NewFrame
-}

----------------------------------------------------------------------
-- registers

type Bus = (Addr -> Ref U8)

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
makeRegister s@State{} name = Ref {onRead,onWrite}
  where
    log = False
    onRead = do
      let v = 0
      when log $ ppuLog s $ printf "%s: read -> %02x" name v
      pure v

    onWrite v = do
      when log $ ppuLog s $ printf "%s: write %02x" name v
      pure ()

ppuLog :: State -> String -> Eff ()
ppuLog State{frame,x,y} message = do
  frame <- read frame
  x <- read x
  y <- read y
  Log $ printf "%d (%3d,%3d) : %s" frame x y message

----------------------------------------------------------------------
-- state

data State = State
  { frame :: Ref Int
  , y :: Ref Int
  , x :: Ref Int
  }

initState :: Eff State
initState = do
  frame <- DefineRegister 0
  y <- DefineRegister (-1)
  x <- DefineRegister 0
  pure State { frame, y, x }

readPosition :: State -> Eff (Int,Int)
readPosition State{x,y} = do
  x <- read x
  y <- read y
  pure (x,y)
