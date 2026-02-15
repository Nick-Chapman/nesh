module Start (main) where

import Bus (makeCpuBus)
import CPU qualified (Config(..),mkState,cpu)
import CommandLine (Config(..),parseConfig)
import Framework (Eff(..),runEffect)
import Graphics qualified (main)
import Mapper (Mapper,loadMapper)
import PPU qualified (initState,makeRegisters,ppu)
import Prelude hiding (read)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let config@Config{rom,sdl} = parseConfig args
  mapper <- loadMapper rom
  let system = makeSystem config mapper
  case sdl of
    False -> do
      let onPlot _ _ _ = pure () -- ignore plottng
      let onFrame = pure False -- never quit
      runEffect onPlot onFrame system
    True -> do
      Graphics.main system

makeSystem :: Config -> Mapper -> Eff ()
makeSystem Config{stop_at,trace_cpu,init_pc} mapper = do
  ppuState <- PPU.initState mapper
  let ppuRegiserBus = PPU.makeRegisters ppuState
  bus <- makeCpuBus mapper ppuRegiserBus -- including wram
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
