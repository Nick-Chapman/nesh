module Start (main) where

import CommandLine (Config(..),parseConfig)
import Controller (makeKeys)
import Framework (Eff(..),runEffect)
import Graphics qualified (main)
import Mapper (initMapper)
import PPU qualified (Graphics(..))
import System (makeSystem)
import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Data.ByteString as BS (readFile,unpack)

main :: IO ()
main = do
  args <- getArgs
  let config@Config{rom=path,sdl} = parseConfig args
  bytes <- BS.unpack <$> BS.readFile path
  let mapperE = initMapper bytes
  case sdl of
    False -> do
      let
        system = do
          tab <- DefineRegister False
          let plot _ _ _ = pure () -- ignore plottng
          let displayFrame n = Print (printf ".%s" (show n))
          --let displayFrame _ = Print "."
          let graphics = PPU.Graphics { plot, displayFrame }
          keys <- Controller.makeKeys
          makeSystem config mapperE tab keys graphics

      runEffect system
      --putStr "\n"
    True -> do
      Graphics.main config mapperE
