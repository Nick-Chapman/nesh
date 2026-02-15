module Start (main) where

import Bus (makeCpuBus)
import CHR qualified (ROM)
import CPU qualified (Config(..),mkState,cpu)
import CommandLine (Config(..),parseConfig)
import Framework (Eff(..),runEffect)
import Graphics qualified (main)
import NesFile (NesFile(..),loadNesFile)
import PPU qualified (initState,makeRegisters,ppu)
import PRG qualified (ROM)
import Prelude hiding (read)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let config@Config{rom,sdl} = parseConfig args
  nesfile <- loadNesFile rom
  let system = makeSystem config nesfile
  case sdl of
    False -> do
      let onPlot _ _ _ = pure () -- ignore plottng
      let onFrame = pure False -- never quit
      runEffect onPlot onFrame system
    True -> do
      Graphics.main system

makeSystem :: Config -> NesFile -> Eff ()
makeSystem Config{stop_at,trace_cpu,init_pc} nesfile = do
  let prg = prgOfNesFile nesfile
  let chr = chrOfNesFile nesfile
  ppuState <- PPU.initState chr
  let ppuRegiserBus = PPU.makeRegisters ppuState
  bus <- makeCpuBus prg ppuRegiserBus -- including wram
  cpuState <- CPU.mkState bus
  --let _peekCpuCyc = CPU.peekCYC cpuState
  let ppu = PPU.ppu ppuState
  let
    cpuConfig = CPU.Config
      { trace = trace_cpu
      , stop_at = stop_at
      , init_pc = init_pc
      }
  let cpu = CPU.cpu cpuConfig cpuState ppuState
  Parallel cpu ppu

prgOfNesFile :: NesFile -> PRG.ROM
prgOfNesFile NesFile{prgs} =
  case prgs of
    [prg2] -> prg2
    -- [prg1,prg2] -> (Just prg1, prg2)
    _  ->
      error $ "emu, unexpected number of prg: " <> show (length prgs)

chrOfNesFile :: NesFile -> CHR.ROM
chrOfNesFile NesFile{chrs} =
  case chrs of
    [chr] -> chr
    _  -> error "emu, unexpected number of chr"
