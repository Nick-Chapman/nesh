module PPU
  ( State, initState, readPosition
  , makeRegisters
  , ppu
  ) where

import Control.Monad (when)
import Prelude hiding (read)
import Framework (Eff(..),Ref(..),read,write,update)
import Types (Addr,U8)
import Text.Printf (printf)

ppu :: State -> Eff ()
ppu s = loop
  where
    loop :: Eff ()
    loop = do
      AdvancePPU 1
      tickPixel s
      loop

tickPixel :: State -> Eff ()
tickPixel State{frame,y,x} = do
  update (+1) x
  xv <- read x
  when (xv == cyclesPerScanLine) $ do
    write x 0
    update (+1) y
    yv <- read y
    when (yv == numScanLines) $ do
      write y 0
      update (+1) frame
  where
    cyclesPerScanLine = 341
    numScanLines = 262

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
    onRead = do
      let v = 0
      ppuLog s $ printf "%s: read -> %02x" name v
      pure v

    onWrite v = do
      ppuLog s $ printf "%s: write %02x" name v
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
  y <- DefineRegister 0
  x <- DefineRegister 0
  pure State { frame, y, x }

readPosition :: State -> Eff (Int,Int)
readPosition State{x,y} = do
  x <- read x
  y <- read y
  pure (x,y)
