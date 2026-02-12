module PPU (ppu) where

import Prelude hiding (read)
import Framework (Eff(..))

ppu :: Eff () -- this is just a placeholder
ppu = loop 0
  where
    loop :: Int -> Eff ()
    loop i = do
      --Log (show ("PPU, tick=",i))
      AdvancePPU 1
      loop (i+1)
