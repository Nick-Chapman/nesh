module System (makeSystem) where -- The full system under emulatiion

import Bus (makeCpuBus)
import CPU qualified (Config(..),mkState,cpu)
import CommandLine (Config(..))
import Framework (Eff(..))
import Mapper (Mapper)
import PPU qualified (initState,makeRegisters,ppu,Graphics(..))

makeSystem :: Config -> Mapper -> PPU.Graphics -> Eff ()
makeSystem Config{stop_at,trace_cpu,init_pc} mapper graphics = do
  ppuState <- PPU.initState mapper graphics
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
