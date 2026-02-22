module System (makeSystem) where -- The full system under emulatiion

import CPU qualified (cpu,mkState,trigger,Interrupt(NMI))
import CommandLine (Config(..))
import Control.Monad (when)
import Controller (Keys,State,initState,makeRegister)
import Framework (Eff(..),Ref,Bus,dummyRef,dummyRef_quiet)
import Mapper (Mapper)
import Mapper qualified (busCPU,busPPU)
import PPU qualified (ppu,initState,Graphics)
import Text.Printf (printf)
import qualified PPU (State,registers,oamDMA)

makeSystem  :: Config -> Eff Mapper -> Ref Bool -> Keys -> PPU.Graphics -> Eff ()
makeSystem config mapperE tab keys graphics = do
  controllerState <- Controller.initState keys
  mapper <- mapperE
  extraCpuCycles <- DefineRegister 0
  ppuBus <- makePpuBus mapper
  ppuState <- PPU.initState ppuBus tab extraCpuCycles
  cpuBus <- makeCpuBus mapper ppuState controllerState
  cpuState <- CPU.mkState extraCpuCycles cpuBus
  let cpu = CPU.cpu config cpuState
  let triggerNMI = CPU.trigger cpuState CPU.NMI
  let ppu = PPU.ppu config triggerNMI ppuState graphics
  Parallel cpu ppu

makeCpuBus :: Mapper -> PPU.State -> Controller.State -> Eff Bus
makeCpuBus mapper ppuState controllerState = do
  wram <- DefineMemory 2048

  let
    cpuBus a = do
      --when (a == 0xfffa) $ Log "Reading from NMI interrupt vector"
      --when (a == 0xfffc) $ Log "Reading from Reset interrupt vector"
      when (a == 0xfffe) $ Log "Reading from IRQ/BRK interrupt vector"
      if
        | a <= 0x07ff -> pure $ wram (fromIntegral a)
        | a >= 0x0800 && a <= 0x0fff -> pure $ wram (fromIntegral a - 0x800)
        | a >= 0x2000 && a <= 0x2007 -> PPU.registers ppuState a

        -- The next range should be mirrors for the PPU registers. Referenced by ice game.
        | a >= 0x2008 && a <= 0x3fff -> pure $ dummyRef "[$2008..$3fff]" a

        | a >= 0x4000 && a <= 0x4013 -> pure $ dummyRef_quiet "APU register" a
        | a == 0x4014 -> pure $ PPU.oamDMA ppuState cpuBus
        | a == 0x4015 -> pure $ dummyRef_quiet "APU status/control" a
        | a == 0x4016 -> pure $ Controller.makeRegister controllerState
        | a == 0x4017 -> pure $ dummyRef_quiet "controller-port2" a

        -- The next range can be responded to by the mapper/cartidge. Referenced by ice game.
        | a >= 0x4020 && a <= 0x7fff -> pure $ dummyRef "[$4020..$7fff]" a -- ice

        | a >= 0x8000 && a <= 0xffff -> Mapper.busCPU mapper a
        | otherwise -> error $ printf "CpuBus: unknown address = $%04X" a

  pure cpuBus


makePpuBus :: Mapper -> Eff Bus
makePpuBus mapper = do
  vram <- DefineMemory 4096 -- TODO: really only 2K, with mirroring
  paletteRam <- DefineMemory 32

  let
    ppuBus :: Bus
    ppuBus a = if
      | a <= 0x1fff -> Mapper.busPPU mapper a

      -- VRAM
      | a >= 0x2000 && a <= 0x2fff -> pure $ vram (fromIntegral a - 0x2000)
      -- | a >= 0x3000 && a <= 0x3eff -> vram (fromIntegral a - 0x3000) --TODO: mirror

      -- palette RAM (weird mirrors)
      | a == 0x3f10 -> ppuBus 0x3f00
      | a == 0x3f14 -> ppuBus 0x3f04
      | a == 0x3f18 -> ppuBus 0x3f08
      | a == 0x3f1c -> ppuBus 0x3f0c

      -- palette RAM
      | a >= 0x3f00 && a <= 0x3f1f -> pure $ paletteRam (fromIntegral a - 0x3f00)
      -- | a >= 0x3f20 && a <= 0x3fff -> paletteRam ((a - 0x3f20) `mod` 0x20) -- TODO: mirrors

      | otherwise -> do
          error $ printf "PpuBus: unknown address = $%04X" a

  pure ppuBus
