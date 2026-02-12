module PPU
  ( State, initState, readPosition
  , makeRegisters
  , ppu
  ) where

import Control.Monad (when)
import Framework (Eff(..),Ref(..),read,write,update)
import Prelude hiding (read)
import Text.Printf (printf)
import Types (Addr,U8,RGB(..))

ppu :: State -> Eff ()
ppu s = loop
  where
    loop :: Eff ()
    loop = do
      AdvancePPU 1
      tickPixel s
      render s
      loop

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

render :: State -> Eff ()
render s@State{x,y} = do
  y <- read y
  x <- read x
  when (visible (x,y)) $ gradientPlot s

visible :: (Int,Int) -> Bool
visible (x,y) = (x >= 0 && x < 256) && (y >= 0 && y < 240)

gradientPlot :: State -> Eff ()
gradientPlot State{frame,x,y} = do
  frame <- read frame
  x <- read x
  y <- read y
  when (not $ visible (x,y)) $ error (show ("gradientPlot",x,y))
  x <- pure $ fromIntegral x
  y <- pure $ fromIntegral y
  let r = 0
  let g = y + fromIntegral frame
  let b = x + fromIntegral frame
  Plot (x,y) RGB { r,g,b }

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
