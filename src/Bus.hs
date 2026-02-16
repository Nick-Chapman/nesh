module Bus (Bus,makeCpuBus) where

import Control.Monad (when)
import Framework (Eff(..),Bus)
import Mapper (Mapper)
import Mapper qualified (busCPU)
import Prelude hiding (read,and,compare)
import Text.Printf (printf)

makeCpuBus :: Mapper -> Bus -> Eff Bus
makeCpuBus mapper ppuRegisterBus = do
  wram <- DefineMemory 2048
  pure $ \a -> do
    when (a == 0xfffa) $ Log "Reading from NMI interrupt vector\n"
    when (a == 0xfffc) $ Log "Reading from Reset interrupt vector\n"
    when (a == 0xfffe) $ Log "Reading from IRQ/BRK interrupt vector\n"
    if
      | a <= 0x07ff -- TODO mirrors
        -> pure $ wram (fromIntegral a)

      | a >= 0x2000 && a <= 0x2007 -- TODO: mirrors
        -> ppuRegisterBus a

      | a >= 0xc000 && a <= 0xffff
        -> pure $ Mapper.busCPU mapper (a - 0xC000)

      | otherwise ->
        error $ printf "makeCpuBus: address = $%04X" a
