module CHR -- TODO: do I really need different modules for PRG and CHR rom
  ( ROM, init, read
  ) where

import Prelude hiding (init,read)

import Data.Array (Array,(!),listArray)
import Types (Addr,U8)

data ROM = ROM { bytesA :: Array Addr U8 }

size :: Addr
size = 0x2000 -- 8k

init :: [U8] -> ROM
init bytes = if
  | n == size -> ROM { bytesA = listArray (0,size-1) bytes }
  | otherwise -> error $ "CHR.init: " <> show n
  where
    n = fromIntegral $ length bytes

read :: ROM -> Addr -> U8
read rom a = if
  | inRange a -> bytesA rom ! a
  | otherwise -> error $ "CHR.read: " <> show a

inRange :: Addr -> Bool
inRange a = a >= 0 && a < size
