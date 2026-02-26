module Start (main) where

import CommandLine (Config(..),parseConfig)
import Controller (makeKeys)
import Framework (Eff(..),runEffect)
import Graphics qualified (main)
import Mapper (initMapper)
import PPU qualified (Graphics(..),makeHack)
import System (makeSystem)
import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Data.ByteString as BS (readFile,unpack)
import Perf (explorePerf)

main :: IO ()
main = do
  args <- getArgs
  let config@Config{rom=path,no_sdl,perf_mode} = parseConfig args
  bytes <- BS.unpack <$> BS.readFile path
  let mapperE = initMapper bytes
  case perf_mode of
    True -> explorePerf config mapperE
    False -> do
      case no_sdl of
        True -> do
          let
            system = do
              hack <- PPU.makeHack
              let plot _ _ _ = pure () -- ignore plottng
              let displayFrame n = Print (printf ".%s" (show n))
              --let displayFrame _ = Print "."
              let graphics = PPU.Graphics { plot, displayFrame }
              keys <- Controller.makeKeys
              makeSystem config mapperE hack keys graphics

          runEffect system
          --putStr "\n"
        False -> do
          Graphics.main config mapperE
