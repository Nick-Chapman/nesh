module System (makeSystem) where -- The full system under emulatiion

import CPU qualified (cpu,mkState,trigger,Interrupt(NMI))
import CommandLine (Config(..))
import Control.Monad (when)
import Controller (Keys,State,initState,makeRegister)

import Framework (Bus,dummyRef,dummyRef_quiet,
                  Eff,defineRegister,defineMemory,log,parallel)

import Mapper (Mapper)
import Mapper qualified (busCPU,busPPU,mirroring)
import PPU qualified (ppu,initState,Graphics,Hack)
import Text.Printf (printf)
import Types (Mirroring(..))
import qualified PPU (State,registers,oamDMA)
import Prelude hiding (log)

makeSystem  :: Config -> Eff Mapper -> PPU.Hack -> Keys -> PPU.Graphics -> Eff ()
makeSystem config mapperE hack keys graphics = do
  controllerState <- Controller.initState keys
  mapper <- mapperE
  extraCpuCycles <- defineRegister 0
  ppuBus <- makePpuBus mapper
  ppuState <- PPU.initState ppuBus hack extraCpuCycles
  cpuBus <- makeCpuBus mapper ppuState controllerState
  cpuState <- CPU.mkState extraCpuCycles cpuBus
  let cpu = CPU.cpu config cpuState
  let triggerNMI = CPU.trigger cpuState CPU.NMI
  let ppu = PPU.ppu config triggerNMI ppuState graphics
  parallel cpu ppu

makeCpuBus :: Mapper -> PPU.State -> Controller.State -> Eff Bus
makeCpuBus mapper ppuState controllerState = do
  wram <- defineMemory 2048

  let
    cpuBus a = do
      --when (a == 0xfffa) $ Log "Reading from NMI interrupt vector"
      --when (a == 0xfffc) $ Log "Reading from Reset interrupt vector"
      when (a == 0xfffe) $ log "Reading from IRQ/BRK interrupt vector"
      if
        | a <= 0x07ff -> pure $ wram (fromIntegral a)
        | a >= 0x0800 && a <= 0x0fff -> pure $ wram (fromIntegral a - 0x800)
        | a >= 0x2000 && a <= 0x2007 -> PPU.registers ppuState a

        -- The next range should be mirrors for the PPU registers. Referenced by ice game.
        | a >= 0x2008 && a <= 0x3fff -> pure $ dummyRef "CPU[$2008..$3fff]" a

        | a >= 0x4000 && a <= 0x4013 -> pure $ dummyRef_quiet "APU register" a
        | a == 0x4014 -> pure $ PPU.oamDMA ppuState cpuBus
        | a == 0x4015 -> pure $ dummyRef_quiet "APU status/control" a
        | a == 0x4016 -> pure $ Controller.makeRegister controllerState
        | a == 0x4017 -> pure $ dummyRef_quiet "controller-port2" a

        -- The next range can be responded to by the mapper/cartidge. Referenced by ice game.
        | a >= 0x4020 && a <= 0x7fff -> pure $ dummyRef "CPU[$4020..$7fff]" a -- ice/castle

        | a >= 0x8000 && a <= 0xffff -> Mapper.busCPU mapper a
        | otherwise -> error $ printf "CpuBus: unknown address = $%04X" a

  pure cpuBus

makePpuBus :: Mapper -> Eff Bus
makePpuBus mapper = do

  -- In total VRAM is 2k. Each half (A,B) is large enough for a single name table.
  vramA <- defineMemory 1024
  vramB <- defineMemory 1024

  let
    -- Four virtual name tables are mirrored into the physical A/B spaces
    -- as determined by the mirroring type selected by the Mapper.
    (nt1,nt2,nt3,nt4) =
      case Mapper.mirroring mapper of
        Horizontal ->
          -- Horizontal mirroring used for games with Vertical scrolling.
          ( vramA, vramA
          , vramB, vramB
          )
        Vertical ->
          -- Vertical mirroring used for games with Horizontal scrolling.
          ( vramA, vramB
          , vramA ,vramB
          )

  paletteRam <- defineMemory 32

  let
    ppuBus :: Bus
    ppuBus a = if

      -- Two pattern tables
      | a <= 0x1fff -> Mapper.busPPU mapper a

      -- VRAM (4 Name Tables)
      | a >= 0x2000 && a <= 0x23ff -> pure $ nt1 (fromIntegral a - 0x2000)
      | a >= 0x2400 && a <= 0x27ff -> pure $ nt2 (fromIntegral a - 0x2400)
      | a >= 0x2800 && a <= 0x2Bff -> pure $ nt3 (fromIntegral a - 0x2800)
      | a >= 0x2C00 && a <= 0x2fff -> pure $ nt4 (fromIntegral a - 0x2C00)

      -- | a >= 0x3000 && a <= 0x3eff -> vram (fromIntegral a - 0x3000) --TODO: mirror

      -- Palette RAM (weird mirrors)
      | a == 0x3f10 -> ppuBus 0x3f00
      | a == 0x3f14 -> ppuBus 0x3f04
      | a == 0x3f18 -> ppuBus 0x3f08
      | a == 0x3f1c -> ppuBus 0x3f0c

      -- Palette RAM
      | a >= 0x3f00 && a <= 0x3f1f -> pure $ paletteRam (fromIntegral a - 0x3f00)
      -- | a >= 0x3f20 && a <= 0x3fff -> paletteRam ((a - 0x3f20) `mod` 0x20) -- TODO: mirrors

      | otherwise -> do
          error $ printf "PpuBus: unknown address = $%04X" a

  pure ppuBus
