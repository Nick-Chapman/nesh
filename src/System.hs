module System (makeSystem) where -- The full system under emulatiion

import CPU qualified (cpu,mkState,trigger,Interrupt(NMI))
import CommandLine (Config(..))
import Control.Monad (when)
import Framework (Eff(..),Ref,Bus,dummyRef_quiet)
import Mapper (Mapper)
import Mapper qualified (busCPU)
import PPU qualified (ppu,initState,Mode,Graphics)
import Text.Printf (printf)
import qualified PPU (State,registers,oamDMA)

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

makeCpuBus :: Mapper -> PPU.State -> Eff Bus
makeCpuBus mapper ppuState = do
  wram <- DefineMemory 2048

  let
    cpuBus a = do
      --when (a == 0xfffa) $ Log "Reading from NMI interrupt vector"
      when (a == 0xfffc) $ Log "Reading from Reset interrupt vector"
      when (a == 0xfffe) $ Log "Reading from IRQ/BRK interrupt vector"
      if
        -- TODO: mirrors
        | a <= 0x07ff -> pure $ wram (fromIntegral a)
        | a >= 0x2000 && a <= 0x2007 -> PPU.registers ppuState a
        | a >= 0x4000 && a <= 0x4013 -> pure $ dummyRef_quiet "APU register" a
        | a == 0x4014 -> pure $ PPU.oamDMA ppuState cpuBus
        | a == 0x4015 -> pure $ dummyRef_quiet "APU status/control" a
        | a == 0x4016 -> pure $ dummyRef_quiet "controller-port1" a
        | a == 0x4017 -> pure $ dummyRef_quiet "controller-port2" a
        | a >= 0xc000 && a <= 0xffff -> pure $ Mapper.busCPU mapper (a - 0xC000)
        | otherwise -> error $ printf "makeCpuBus: address = $%04X" a

  pure cpuBus
