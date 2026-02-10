
module Types
  ( U8
  , HL(..)
  , Addr, makeAddr, splitAddr
  , Flag(..), testFlag,updateFlag
  ) where

import Data.Word (Word8,Word16)
import Data.Bits (shiftL, (.|.),testBit,setBit,clearBit)

type Addr = U16

type U16 = Word16
type U8 = Word8

data HL a = HL { hi :: a, lo :: a}

makeAddr :: HL U8 -> Addr
makeAddr HL{hi,lo} = fromIntegral hi `shiftL` 8 .|. fromIntegral lo

splitAddr :: Addr -> HL U8
splitAddr = undefined

data Flag = C | Z | N

flagBitNum :: Flag -> Int
flagBitNum = \case
  C -> 1
  Z -> 1
  N -> 7

testFlag :: U8 -> Flag -> Bool
testFlag v flag  = v `testBit` (flagBitNum flag)

updateFlag :: Flag -> Bool -> U8 -> U8
updateFlag flag bool v = (if bool then setBit else clearBit) v (flagBitNum flag)


