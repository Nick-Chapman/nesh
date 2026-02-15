module System (makeSystem) where -- The full system under emulatiion

import Bus (makeCpuBus)
import CPU qualified (mkState,cpu)
import CommandLine (Config(..))
import Framework (Eff(..))
import Mapper (Mapper)
import PPU qualified (State,makeRegisters,ppu,Graphics(..))

makeSystem :: Config -> Mapper -> PPU.State -> PPU.Graphics -> Eff ()
makeSystem config mapper ppuState graphics = do
  let ppuRegiserBus = PPU.makeRegisters ppuState
  bus <- makeCpuBus mapper ppuRegiserBus -- including wram
  cpuState <- CPU.mkState bus
  --let _peekCpuCyc = CPU.peekCYC cpuState
  let cpu = CPU.cpu config cpuState ppuState
  let ppu = PPU.ppu ppuState graphics
  Parallel cpu ppu
