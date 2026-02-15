module System (makeSystem) where -- The full system under emulatiion

import Bus (makeCpuBus)
import CPU qualified (mkState,cpu)
import CommandLine (Config(..))
import Framework (Eff(..))
import Mapper (Mapper)
import PPU qualified (initState,makeRegisters,ppu,Graphics(..))

makeSystem :: Config -> Mapper -> PPU.Graphics -> Eff ()
makeSystem config mapper graphics = do
  ppuState <- PPU.initState mapper graphics
  let ppuRegiserBus = PPU.makeRegisters ppuState
  bus <- makeCpuBus mapper ppuRegiserBus -- including wram
  cpuState <- CPU.mkState bus
  --let _peekCpuCyc = CPU.peekCYC cpuState
  let ppu = PPU.ppu ppuState
  let cpu = CPU.cpu config cpuState ppuState
  Parallel cpu ppu
