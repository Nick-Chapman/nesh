
module Types
  ( U8
  , HL(..)
  , Addr, makeAddr, splitAddr
  , Flag(..), testFlag,updateFlag
  ) where

import Data.Word (Word8,Word16)
import Data.Bits (shiftL,shiftR,(.|.),(.&.),testBit,setBit,clearBit)

type Addr = U16

type U16 = Word16
type U8 = Word8

data HL a = HL { hi :: a, lo :: a}

makeAddr :: HL U8 -> Addr
makeAddr HL{hi,lo} = fromIntegral hi `shiftL` 8 .|. fromIntegral lo

splitAddr :: Addr -> HL U8
splitAddr addr = do
  HL { hi = fromIntegral (addr `shiftR` 8), lo = fromIntegral (addr .&. 0xff) }

data Flag = C | Z | I | D | V | N

flagBitNum :: Flag -> Int
flagBitNum = \case
  C -> 0
  Z -> 1
  I -> 2
  D -> 3
  -- 4,5
  V -> 6
  N -> 7

testFlag :: U8 -> Flag -> Bool
testFlag v flag  = v `testBit` (flagBitNum flag)

updateFlag :: Flag -> Bool -> U8 -> U8
updateFlag flag bool v = (if bool then setBit else clearBit) v (flagBitNum flag)
