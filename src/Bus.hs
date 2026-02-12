
module Bus (Bus,makeCpuBus) where

import Framework (Eff(..),Ref(..))
import PRG qualified (ROM,read)
import Prelude hiding (read,and,compare)
import Text.Printf (printf)
import Types (U8,Addr)

type Bus = (Addr -> Ref U8)

makeCpuBus :: PRG.ROM -> Bus -> Eff Bus
makeCpuBus prg ppuRegisterBus = do
  wram <- DefineMemory 2048
  pure $ \a -> do
    if
      | a <= 0x07ff -- TODO mirrors
        -> wram (fromIntegral a)

      | a >= 0x2000 && a <= 0x2007 -- TODO: mirrors
        -> ppuRegisterBus a

      | a >= 0xc000 && a <= 0xffff
        -> readPRG prg (a - 0xC000)

      | otherwise ->
        error $ printf "makeCpuBus: address = $%04X" a

readPRG :: PRG.ROM -> Addr -> Ref U8
readPRG prg a = readonly (PRG.read prg a)
  where
    readonly :: U8 -> Ref U8
    readonly byte =
      Ref { onRead = pure byte
          , onWrite = \v -> error (show ("readonly/onWrite",a,v))
          }
