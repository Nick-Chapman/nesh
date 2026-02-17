module System (makeSystem) where -- The full system under emulatiion

import Bus (makeCpuBus)
import CPU qualified (cpu,mkState,trigger,Interrupt(NMI))
import CommandLine (Config(..))
import Framework (Eff(..),Ref)
import Mapper (Mapper)
import PPU qualified (ppu,makeRegisters,initState,Mode,Graphics)

makeSystem  :: Config -> Mapper -> Ref PPU.Mode -> PPU.Graphics -> Eff ()
makeSystem config mapper mode graphics = do
  ppuState <- PPU.initState mapper mode
  let ppuRegisers = PPU.makeRegisters ppuState
  bus <- makeCpuBus mapper ppuRegisers
  cpuState <- CPU.mkState bus
  let cpu = CPU.cpu config cpuState
  let triggerNMI = CPU.trigger cpuState CPU.NMI
  let ppu = PPU.ppu triggerNMI ppuState graphics
  Parallel cpu ppu
