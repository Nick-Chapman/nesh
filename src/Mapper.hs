module Mapper
  ( Mapper, initMapper
  , busCPU, busPPU
  ) where

import Prelude hiding (read)
import Control.Monad (when)
import Data.Array (Array,listArray,(!))
import Data.Bits ((.&.),(.|.),shiftR) -- testBit
import Data.ByteString.Internal (w2c)
import Framework (Ref(..),Eff(..),Bus,read,write)
import Text.Printf (printf)
import Types (U8)

data Mapper = Mapper
  { busCPU :: Bus
  , busPPU :: Bus
  }

noWrite :: String -> U8 -> Eff ()
noWrite tag = \_ -> Error (printf "unexpected write to Rom: %s" tag)

initMapper :: [U8] -> Eff Mapper
initMapper bs = do

  let
    makeRom :: (U8 -> Eff ()) -> Int -> Int -> (Array Int (Ref U8))
    makeRom onWrite size starting =
      listArray (0, size -1) [ Ref { onRead = pure b, onWrite } | b <- drop starting bs ]

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

  let actualSize = length bs
  let expectedSize = headerSize + (x * prgSize) + (y * chrSize)

  when (actualSize /= expectedSize) $ do
    Log $ printf "bad file size: (mapper=%d,x=%d,y=%d) actual=%d, expected=%d, diff=%d"
      mapperNumber x y
      actualSize expectedSize (actualSize-expectedSize)

  case mapperNumber of
    0 -> do
      when (y /= 1) $ error $ printf "mapper0: unexpected number of CHR roms: %d\n" y
      let chr = makeRom (noWrite "mapper0/chr") chrSize (headerSize + x * prgSize)
      let busPPU :: Bus = \a -> pure $ do chr ! fromIntegral a
      let
        (prg1,prg2) = if
          | x == 1 -> do -- NROM-128
              let prg = makeRom (noWrite "mapper0/prg") prgSize headerSize
              (prg,prg) -- mirroring

          | x == 2 -> do -- NROM-256
              let prg1 = makeRom (noWrite "mapper0/prg1") prgSize headerSize
              let prg2 = makeRom (noWrite "mapper0/prg2") prgSize (headerSize + prgSize)
              (prg1,prg2)

          | otherwise ->
              error "mapper0: unexpected number of PRG roms"

      let
        busCPU :: Bus
        busCPU a = if
          | a >= 0x8000 && a <= 0xBfff -> pure $ prg1 ! (fromIntegral a - 0x8000)
          | a >= 0xC000 && a <= 0xffff -> pure $ prg2 ! (fromIntegral a - 0xC000)
          | otherwise -> error $ printf "Mapper0(busCPU): address = $%04X" a

      pure Mapper { busPPU, busCPU }

    2 -> do
      when (y /= 0) $ error (printf "Mapper2: y=%d, but only CHR RAM supported (y=0)" y)
      chr <- DefineMemory chrSize
      let
        busPPU :: Bus
        busPPU a = if
          | a >= 0x0000 && a <= 0x1fff -> pure $ chr (fromIntegral a)
          | otherwise -> error $ printf "Mapper(busPPU) : address = $%04X" a

      bankSelectRegister <- DefineRegister 0

      let

      let
        onWrite v = do
          when (v > 7) $ error (printf "Mapper2: bank select too-big: %d" v)
          write v bankSelectRegister
          pure ()

      -- TODO: just make one big rom, determined by x
      let prg0 = makeRom onWrite prgSize (headerSize)
      let prg1 = makeRom onWrite prgSize (headerSize + 1 * prgSize)
      let prg2 = makeRom onWrite prgSize (headerSize + 2 * prgSize)
      let prg3 = makeRom onWrite prgSize (headerSize + 3 * prgSize)
      let prg4 = makeRom onWrite prgSize (headerSize + 4 * prgSize)
      let prg5 = makeRom onWrite prgSize (headerSize + 5 * prgSize)
      let prg6 = makeRom onWrite prgSize (headerSize + 6 * prgSize)
      let prg7 = makeRom onWrite prgSize (headerSize + 7 * prgSize)

      let
        bank :: U8 -> Array Int (Ref U8)
        bank = \case
          0 -> prg0
          1 -> prg1
          2 -> prg2
          3 -> prg3
          4 -> prg4
          5 -> prg5
          6 -> prg6
          7 -> prg7
          x -> error (show ("bank",x))

      let
        busCPU :: Bus
        busCPU a = if
          | a >= 0x8000 && a <= 0xBfff -> do
              v <- read bankSelectRegister
              pure (bank v ! (fromIntegral a - 0x8000))

          | a >= 0xC000 && a <= 0xffff ->
              pure (prg7 ! (fromIntegral a - 0xC000))

          | otherwise -> do
              error $ printf "Mapper(busCPU) : address = $%04X" a

      pure Mapper { busPPU, busCPU }


    _ -> do
      error $ printf "Unsupported mapper number: %d\n" mapperNumber
