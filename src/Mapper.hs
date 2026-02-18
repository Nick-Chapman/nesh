module Mapper
  ( Mapper, loadMapper
  , busCPU, busPPU
  ) where

import Control.Monad (when)
import Data.Array (listArray,(!))
import Data.Bits ((.&.),(.|.),shiftR) -- testBit
import Data.ByteString.Internal (w2c)
import Framework (Ref(..))
import Text.Printf (printf)
import Types (U8,Addr)
import qualified Data.ByteString as BS (readFile,unpack)

data Mapper = Mapper
  { busCPU :: Addr -> Ref U8
  , busPPU :: Addr -> Ref U8
  }

loadMapper :: String -> IO Mapper
loadMapper path = do
  byteString <- BS.readFile path
  let bs = BS.unpack byteString
  let
    headerSize = 16
    prgSize = 0x4000 --16k
    chrSize = 0x2000 --8k

  when (length bs < headerSize) $ error "header failure, too short"
  when (map w2c (take 3 bs) /= "NES") $ error "header failure, missing NES tag"

  let x = fromIntegral (bs !! 4)
  let y = fromIntegral (bs !! 5)
  let byte6 = bs !! 6
  let byte7 = bs !! 7
  let mapperNumber = byte7 .&. 0xf0 .|. byte6 `shiftR` 4

  --let ntm = if byteToUnsigned (bs !! 6) `testBit` 0 then NTM_Horizontal else NTM_Vertical

  when (mapperNumber /= 0) $ printf "Unsupport mapper number: %d\n" mapperNumber
  when (length bs /= headerSize + (x * prgSize) + (y * chrSize)) $ printf "bad file size\n"

  when (y /= 1) $ printf "unexpected number of CHR roms: %d\n" y

  let
    mappedPrgAddress =
      if
        | x == 1 -> 0xC000 -- NROM-128
        | x == 2 -> 0x8000 -- NROM-256
        | otherwise -> error "unexpected number of PRG roms"

  let arr = listArray (0, length bs -1)
        [ Ref { onRead, onWrite }
        | b <- bs
        , let onRead = pure b
        , let onWrite _ = pure ()
        ]

  let prgStart = headerSize
  let chrStart = prgStart + x * prgSize

  let busCPU a = arr ! (fromIntegral a + prgStart - mappedPrgAddress)
  let busPPU a = arr ! (fromIntegral a + chrStart)

  pure Mapper { busPPU, busCPU }
