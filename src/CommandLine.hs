module CommandLine (main) where

import CPU qualified (Config(..),cpu)
import Framework (Eff(..),runEffect)
import NesFile (NesFile(..),loadNesFile)
import PPU (ppu)
import PRG qualified (ROM)
import Prelude hiding (read)
import Prelude qualified
import System.Environment (getArgs)
import Text.Printf (printf)
import Types (Addr)

main :: IO ()
main = do
  args <- getArgs
  let config@Config{rom} = parseConfig args
  nesfile <- loadNesFile rom
  let prg = prgOfNesFile nesfile
  runEffect (system config prg)

prgOfNesFile :: NesFile -> PRG.ROM
prgOfNesFile NesFile{prgs} =
  case prgs of
    [prg2] -> prg2
    -- [prg1,prg2] -> (Just prg1, prg2)
    _  ->
      error $ "emu, unexpected number of prg: " <> show (length prgs)

system :: Config -> PRG.ROM -> Eff ()
system Config{stop_at,trace_cpu,init_pc} prg = do
  let cpu = CPU.cpu CPU.Config { trace = trace_cpu
                               , stop_at = stop_at
                               , init_pc = init_pc
                               } prg
  Parallel cpu ppu

data Config = Config
  { rom :: FilePath
  , trace_cpu :: Bool
  , stop_at :: Maybe Int
  , init_pc :: Maybe Addr -- Nothing means use reset vector
  }

parseConfig :: [String] -> Config
parseConfig = loop config0
  where
    config0 = Config
      { rom = "[some.rom]"
      , trace_cpu = False
      , stop_at = Nothing
      , init_pc = Nothing
      }
    loop :: Config -> [String] -> Config
    loop acc = \case
      [] -> acc
      "--trace-cpu":rest -> loop acc { trace_cpu = True } rest
      "--stop-at":n:rest -> loop acc { stop_at = Just (Prelude.read n) } rest
      "--init-pc":n:rest -> loop acc { init_pc = Just (Prelude.read n) } rest
      flag@('-':_):_ -> error (printf "unknown flag: %s" flag)
      rom:rest -> loop acc { rom } rest
