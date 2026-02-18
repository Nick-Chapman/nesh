module Mapper
  ( Mapper, loadMapper
  , busCPU, busPPU
  ) where

import Control.Monad (when)
import Data.Array (Array,(!),listArray)
import Data.ByteString.Internal (w2c)
import Framework (Eff(..),Ref(..))
import Types (U8,Addr)
import qualified Data.ByteString as BS (readFile,unpack)

data Rom = Rom { arr :: Array Addr U8 }

initRom :: [U8] -> Rom
initRom bytes = Rom { arr = listArray (0,n-1) bytes }
  where n = fromIntegral $ length bytes

readRom :: Rom -> Addr -> U8
readRom Rom{arr} a = arr ! a

data Mapper = Mapper
  { header :: [U8]
  , prgs :: [Rom]
  , chrs :: [Rom]
  }

busCPU :: Mapper -> Addr -> Ref U8
busCPU Mapper{prgs} a = readonly (readRom prg a)
  where
    prg = case prgs of [prg] -> prg; _  -> error $ "emu, unexpected number of prg"
    readonly :: U8 -> Ref U8
    readonly byte =
      Ref { onRead = pure byte
          , onWrite = \v -> Error (show ("busCPU: readonly/onWrite",a,v))
          }

busPPU :: Mapper -> Addr -> Ref U8
busPPU Mapper{chrs} a = readonly (readRom chr a)
  where
    chr = case chrs of [chr] -> chr; _  -> error "emu, unexpected number of chr"
    readonly :: U8 -> Ref U8
    readonly byte =
      Ref { onRead = pure byte
--          , onWrite = \v -> Error (show ("busPPU: readonly/onWrite",a,v))
          , onWrite = \_v -> pure () -- for nestest
          }

loadMapper :: String -> IO Mapper
loadMapper path = do
  byteString <- BS.readFile path
  let bs = BS.unpack byteString
  when (length bs < headerSize) $ error "header failure, too short"
  when (map w2c (take 3 bs) /= "NES") $ error "header failure, missing NES tag"
  let header = take headerSize bs
  let x = fromIntegral (bs !! 4)
  let y = fromIntegral (bs !! 5)
  --let ntm = if byteToUnsigned (bs !! 6) `testBit` 0 then NTM_Horizontal else NTM_Vertical
  when (length bs /= headerSize + (x * prgSize) + (y * chrSize)) $ error "bad file size"
  let prgs = map (\i -> initRom $ take prgSize $ drop (headerSize + i * prgSize) bs) [0..x-1]
  let chrs = map (\i -> initRom $ take chrSize $ drop (headerSize + x * prgSize + i * 2 * patSize) bs) [0..y-1]
  return $ Mapper { header,  prgs, chrs } --,  ntm }
    where
        headerSize = 16
        prgSize = 0x4000 --16k
        patSize = 0x1000 --2k (One PAT of 256 tiles)
        chrSize = 2 * patSize --4k
