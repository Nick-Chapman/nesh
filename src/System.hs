module System (makeSystem) where -- The full system under emulatiion

import Bus (makeCpuBus)
import CPU qualified (cpu,mkState,trigger,Interrupt(NMI))
import CommandLine (Config(..))
import Framework (Eff(..),Ref)
import Mapper (Mapper)
import PPU qualified (ppu,initState,Mode,Graphics)

makeSystem  :: Config -> Mapper -> Ref PPU.Mode -> PPU.Graphics -> Eff ()
makeSystem config mapper mode graphics = do
  extraCpuCycles <- DefineRegister 0
  ppuState <- PPU.initState mapper mode extraCpuCycles
  bus <- makeCpuBus mapper ppuState
  cpuState <- CPU.mkState extraCpuCycles bus
  let cpu = CPU.cpu config cpuState
  let triggerNMI = CPU.trigger cpuState CPU.NMI
  let ppu = PPU.ppu triggerNMI ppuState graphics
  Parallel cpu ppu
