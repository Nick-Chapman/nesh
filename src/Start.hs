module Start (main) where

import CommandLine (Config(..),parseConfig)
import Framework (Eff(..),runEffect)
import Graphics qualified (main)
import Mapper (loadMapper)
import PPU qualified (Graphics(..),initMode)
import System (makeSystem)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  let config@Config{rom,sdl} = parseConfig args
  mapper <- loadMapper rom
  case sdl of
    False -> do
      let
        system = do
          mode <- DefineRegister PPU.initMode
          let plot _ _ _ = pure () -- ignore plottng
          let displayFrame n = Log (printf ".%d" n)
          let graphics = PPU.Graphics { plot, displayFrame }
          makeSystem config mapper mode graphics

      runEffect system
    True -> do
      Graphics.main config mapper
