module Mapper
  ( Mapper, loadMapper
  , busCPU, busPPU
  ) where

import Control.Monad (when)
import Data.Array (Array,listArray,(!))
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

  -- y <- pure $ if (y==0) then 1 else y

  let actualSize = length bs
  let expectedSize = headerSize + (x * prgSize) + (y * chrSize)

  when (mapperNumber /= 0) $ printf "Unsupport mapper number: %d\n" mapperNumber
  when (actualSize /= expectedSize) $ do
    printf "bad file size: (mapper=%d,x=%d,y=%d) actual=%d, expected=%d, diff=%d\n"
      mapperNumber x y
      actualSize expectedSize (actualSize-expectedSize)

  let
    makeRom :: Int -> Int -> Array Int (Ref U8)
    makeRom size starting = listArray (0, size -1)
      [ Ref { onRead, onWrite }
      | b <- drop starting bs
      , let onRead = pure b
      , let onWrite _ = pure ()
      ]

    makeChrRom = makeRom chrSize
    makePrgRom = makeRom prgSize


  case mapperNumber of
    0 -> do

      when (y /= 1) $ error $ printf "mapper0: unexpected number of CHR roms: %d\n" y

      let chr = makeChrRom (headerSize + x * prgSize)
      let busPPU a = chr ! fromIntegral a

      let
        (prg1,prg2) =
          if
            | x == 1 -> do -- NROM-128
                let prg = makePrgRom headerSize
                (prg,prg) -- mirroring

            | x == 2 -> do -- NROM-256
                let prg1 = makePrgRom headerSize
                let prg2 = makePrgRom (headerSize + prgSize)
                (prg1,prg2)

            | otherwise ->
                error "mapper0: unexpected number of PRG roms"

      let
        busCPU :: Addr -> Ref U8
        busCPU a =
          if
            | a >= 0x8000 && a <= 0xBfff -> prg1 ! (fromIntegral a - 0x8000)
            | a >= 0xC000 && a <= 0xffff -> prg2 ! (fromIntegral a - 0xC000)
            | otherwise -> error $ printf "Mapper: address = $%04X" a

      pure Mapper { busPPU, busCPU }

    2 -> do
      undefined


    _ -> do
      error $ printf "Unsupport mapper number: %d\n" mapperNumber
