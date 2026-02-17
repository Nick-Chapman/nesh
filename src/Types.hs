
module Types
  ( U8
  , HL(..)
  , Addr, makeAddr, splitAddr
  , RGB
  ) where

import Data.Word (Word8,Word16)
import Data.Bits (shiftL,shiftR,(.|.),(.&.))
import SDL (V4)

type U8 = Word8
type U16 = Word16

type Addr = U16

data HL a = HL { hi :: a, lo :: a}

makeAddr :: HL U8 -> Addr
makeAddr HL{hi,lo} = fromIntegral hi `shiftL` 8 .|. fromIntegral lo

splitAddr :: Addr -> HL U8
splitAddr addr = do
  HL { hi = fromIntegral (addr `shiftR` 8), lo = fromIntegral (addr .&. 0xff) }

type RGB = V4 U8 -- TODO rename Colour
