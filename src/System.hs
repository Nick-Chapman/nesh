module System (makeSystem) where -- The full system under emulatiion

import Bus (makeCpuBus)
import CPU qualified (mkState,cpu,Interrupt(NMI),trigger)
import CommandLine (Config(..))
import Framework (Eff(..))
import Mapper (Mapper)
import PPU qualified (State,makeRegisters,ppu,Graphics(..))

makeSystem :: Config -> Mapper -> PPU.State -> PPU.Graphics -> Eff ()
makeSystem config mapper ppuState graphics = do
  let ppuRegisers = PPU.makeRegisters ppuState
  bus <- makeCpuBus mapper ppuRegisers
  cpuState <- CPU.mkState bus
  let cpu = CPU.cpu config cpuState
  let triggerNMI = CPU.trigger cpuState CPU.NMI
  let ppu = PPU.ppu triggerNMI ppuState graphics
  Parallel cpu ppu
