module PPU
  ( State, initState, readPosition
  , ppu
  ) where

import Control.Monad (when)
import Prelude hiding (read)
import Framework (Eff(..),Ref,read,write,update)

cyclesPerScanLine :: Int
cyclesPerScanLine = 341

ppu :: State -> Eff ()
ppu s = loop
  where
    loop :: Eff ()
    loop = do
      AdvancePPU 1
      tickPixel s
      loop

data State = State
  { x :: Ref Int
  , y :: Ref Int
  }

initState :: Eff State
initState = do
  x <- DefineRegister 0
  y <- DefineRegister 0
  pure State { x, y }

tickPixel :: State -> Eff ()
tickPixel State{x,y} = do
  update (+1) x
  xv <- read x
  when (xv == cyclesPerScanLine) $ do write x 0; update (+1) y

readPosition :: State -> Eff (Int,Int)
readPosition State{x,y} = do
  x <- read x
  y <- read y
  pure (x,y)
