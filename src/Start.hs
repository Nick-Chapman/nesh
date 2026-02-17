module Start (main) where

import CommandLine (Config(..),parseConfig)
import Framework (Eff(..),runEffect)
import Graphics qualified (main)
import Mapper (loadMapper)
import PPU qualified (Graphics(..),initState,initMode)
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
          ppuState <- PPU.initState mapper mode
          let graphics = PPU.Graphics
                { plot = \_ _ _ -> pure () -- ignore plottng
                , displayFrame = \n -> Log (printf ".%d" n)
                }
          makeSystem config mapper ppuState graphics

      runEffect system
    True -> do
      Graphics.main config mapper
