
module Types
  ( U8
  , HL(..)
  , Addr, makeAddr
  ) where

import Data.Word (Word8,Word16)
import Data.Bits (shiftL, (.|.))

type Addr = U16

type U16 = Word16
type U8 = Word8

data HL a = HL { hi :: a, lo :: a}

makeAddr :: HL U8 -> Addr
makeAddr HL{hi,lo} = fromIntegral hi `shiftL` 8 .|. fromIntegral lo
