
module Types
  ( U8
  , HL(..)
  , Addr, makeAddr, splitAddr
  ) where

import Data.Word (Word8,Word16)
import Data.Bits (shiftL,shiftR,(.|.),(.&.))

type Addr = Word16
type U8 = Word8

data HL a = HL { hi :: a, lo :: a}

makeAddr :: HL U8 -> Addr
makeAddr HL{hi,lo} = fromIntegral hi `shiftL` 8 .|. fromIntegral lo

splitAddr :: Addr -> HL U8
splitAddr addr = do
  HL { hi = fromIntegral (addr `shiftR` 8), lo = fromIntegral (addr .&. 0xff) }
