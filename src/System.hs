module System (makeSystem) where -- The full system under emulatiion

import CPU qualified (cpu,mkState,trigger,Interrupt(NMI))
import CommandLine (Config(..))
import Control.Monad (when)
import Framework (Eff(..),Ref,Bus,dummyRef_quiet)
import Mapper (Mapper)
import Mapper qualified (busCPU,busPPU)
import PPU qualified (ppu,initState,Mode,Graphics)
import Text.Printf (printf)
import qualified PPU (State,registers,oamDMA)

makeSystem  :: Config -> Mapper -> Ref PPU.Mode -> PPU.Graphics -> Eff ()
makeSystem config mapper mode graphics = do
  extraCpuCycles <- DefineRegister 0
  ppuBus <- makePpuBus mapper
  ppuState <- PPU.initState ppuBus mode extraCpuCycles
  cpuBus <- makeCpuBus mapper ppuState
  cpuState <- CPU.mkState extraCpuCycles cpuBus
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
      --when (a == 0xfffc) $ Log "Reading from Reset interrupt vector"
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
        | a >= 0x8000 && a <= 0xffff -> pure $ Mapper.busCPU mapper a
        | otherwise -> error $ printf "makeCpuBus: address = $%04X" a

  pure cpuBus


makePpuBus :: Mapper -> Eff Bus
makePpuBus mapper = do
  vram <- DefineMemory 4096 -- TODO: really only 2K, with mirroring
  paletteRam <- DefineMemory 32
  pure $ \a -> pure $ do
    if
      | a <= 0x1fff -> Mapper.busPPU mapper a

      -- VRAM
      | a >= 0x2000 && a <= 0x2fff -> vram (fromIntegral a - 0x2000)
      -- | a >= 0x3000 && a <= 0x3eff -> vram (fromIntegral a - 0x3000) --TODO: mirror

      -- palette RAM
      | a >= 0x3f00 && a <= 0x3f1f -> paletteRam (fromIntegral a - 0x3f00)
      -- | a >= 0x3f20 && a <= 0x3fff -> paletteRam ((a - 0x3f20) `mod` 0x20) -- TODO: mirrors

      | otherwise -> do
          error $ printf "PpuBus: unknown address = $%04X" a
