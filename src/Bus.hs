module Bus (Bus,makeCpuBus) where

import Framework (Eff(..),Ref(..))
import Mapper (Mapper)
import Mapper qualified (busCPU)
import Prelude hiding (read,and,compare)
import Text.Printf (printf)
import Types (U8,Addr)

type Bus = (Addr -> Ref U8)

makeCpuBus :: Mapper -> Bus -> Eff Bus
makeCpuBus mapper ppuRegisterBus = do
  wram <- DefineMemory 2048
  pure $ \a -> do
    if
      | a <= 0x07ff -- TODO mirrors
        -> wram (fromIntegral a)

      | a >= 0x2000 && a <= 0x2007 -- TODO: mirrors
        -> ppuRegisterBus a

      | a >= 0xc000 && a <= 0xffff
        -> Mapper.busCPU mapper (a - 0xC000)

      | otherwise ->
        error $ printf "makeCpuBus: address = $%04X" a
