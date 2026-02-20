module Start (main) where

import CommandLine (Config(..),parseConfig)
import Framework (Eff(..),runEffect)
import Graphics qualified (main)
import Mapper (initMapper)
import PPU qualified (Graphics(..),initMode)
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
          mode <- DefineRegister PPU.initMode
          let plot _ _ _ = pure () -- ignore plottng
          let _displayFrame n = Log (printf ".%d" n)
          let displayFrame _ = Print "."
          let graphics = PPU.Graphics { plot, displayFrame }
          makeSystem config mapperE tab mode graphics

      runEffect system
    True -> do
      Graphics.main config mapperE
