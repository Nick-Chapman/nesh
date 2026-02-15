module Start (main) where

import CommandLine (Config(..),parseConfig)
import Framework (Eff(..),runEffect)
import Graphics qualified (main)
import Mapper (loadMapper)
import PPU qualified (Graphics(..))
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
      let graphics = PPU.Graphics
            { plot = \_ _ _ -> pure () -- ignore plottng
            , displayFrame = \n -> Log (printf ".%d" n)
            }
      let system = makeSystem config mapper graphics
      runEffect system
    True -> do
      Graphics.main config mapper
