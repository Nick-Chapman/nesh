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
  , no_sdl :: Bool -- dont show graphics
  , stop_frame :: Maybe Int
  , perf_mode :: Bool
  }
  deriving Show

parseConfig :: [String] -> Config
parseConfig = loop config0
  where
    config0 = Config
      { rom = "[some.rom]"
      , trace_cpu = False
      , stop_at = Nothing
      , init_pc = Nothing
      , no_sdl = False
      , stop_frame = Nothing
      , perf_mode = False
      }
    loop :: Config -> [String] -> Config
    loop acc = \case
      [] -> acc
      "--perf":rest -> loop acc { perf_mode = True } rest
      "--no-sdl":rest -> loop acc { no_sdl = True } rest
      "--trace-cpu":rest -> loop acc { trace_cpu = True } rest
      "--stop-at":n:rest -> loop acc { stop_at = Just (Prelude.read n) } rest
      "--init-pc":n:rest -> loop acc { init_pc = Just (Prelude.read n) } rest
      "--stop-frame":n:rest -> loop acc { stop_frame = Just (Prelude.read n) } rest
      flag@('-':_):_ -> error (printf "unknown flag: %s" flag)
      rom:rest -> loop acc { rom } rest
