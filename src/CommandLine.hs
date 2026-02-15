module CommandLine
  ( Config(..)
  , parseConfig
  ) where

import Text.Printf (printf)
import Types (Addr)

data Config = Config
  { rom :: FilePath
  , trace_cpu :: Bool
  , stop_at :: Maybe Int
  , init_pc :: Maybe Addr -- Nothing means use reset vector
  , sdl :: Bool -- show graphics
  }

parseConfig :: [String] -> Config
parseConfig = loop config0
  where
    config0 = Config
      { rom = "[some.rom]"
      , trace_cpu = False
      , stop_at = Nothing
      , init_pc = Nothing
      , sdl = False
      }
    loop :: Config -> [String] -> Config
    loop acc = \case
      [] -> acc
      "--sdl":rest -> loop acc { sdl = True } rest
      "--trace-cpu":rest -> loop acc { trace_cpu = True } rest
      "--stop-at":n:rest -> loop acc { stop_at = Just (Prelude.read n) } rest
      "--init-pc":n:rest -> loop acc { init_pc = Just (Prelude.read n) } rest
      flag@('-':_):_ -> error (printf "unknown flag: %s" flag)
      rom:rest -> loop acc { rom } rest
