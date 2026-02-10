module Top (main) where

import Prelude hiding (read)
import Framework (Eff(..),runEffect)
import CPU (cpu)
import PPU (ppu)
import NesFile (NesFile(..),loadNesFile)
import PRG qualified (ROM)

main :: IO ()
main = do
  print "*emu-framework*"
  nesfile <- loadNesFile "nestest.nes"
  let prg = prgOfNesFile nesfile
  runEffect 22 (system prg)

prgOfNesFile :: NesFile -> PRG.ROM
prgOfNesFile NesFile{prgs} =
  case prgs of
    [prg2] -> prg2
    -- [prg1,prg2] -> (Just prg1, prg2)
    _  ->
      error $ "emu, unexpected number of prg: " <> show (length prgs)

system :: PRG.ROM -> Eff ()
system prg = do
  Parallel (cpu prg) ppu
