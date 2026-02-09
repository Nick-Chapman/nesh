module Top (main) where

import Prelude hiding (read)
import Framework (Eff(..),runEffect)
import CPU (cpu)
import PPU (ppu)

main :: IO ()
main = do
  print "*emu-framework*"
  runEffect 22 system

system :: Eff ()
system = Parallel cpu ppu
