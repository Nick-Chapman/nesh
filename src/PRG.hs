module PRG
  ( ROM, init, read
  ) where

import Prelude hiding (init,read)

import Data.Array (Array,(!),listArray)
import Types (Addr,U8)

data ROM = ROM { bytesA :: Array Addr U8 }

size :: Addr
size = 0x4000 -- 16k

init :: [U8] -> ROM
init bytes = if
  | n == size -> ROM { bytesA = listArray (0,size-1) bytes }
  | otherwise -> error $ "PRG.init: " <> show n
  where
    n = fromIntegral $ length bytes

read :: ROM -> Addr -> U8
read rom a = if
  | inRange a -> bytesA rom ! a
  | otherwise -> error $ "PRG.read: " <> show a

inRange :: Addr -> Bool
inRange a = a >= 0 && a < size
