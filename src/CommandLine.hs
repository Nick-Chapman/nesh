module CommandLine (main) where

import Bus (makeCpuBus)
import CPU qualified (Config(..),cpu)
import Framework (Eff(..),runEffect)
import Graphics qualified (main)
import NesFile (NesFile(..),loadNesFile)
import PPU qualified (initState,makeRegisters,ppu)
import PRG qualified (ROM)
import Prelude hiding (read)
import Prelude qualified
import System.Environment (getArgs)
import Text.Printf (printf)
import Types (Addr)

main :: IO ()
main = do
  args <- getArgs
  let config@Config{rom,sdl} = parseConfig args
  nesfile <- loadNesFile rom
  let prg = prgOfNesFile nesfile
  let system = makeSystem config prg

  case sdl of
    False -> do
      let onPlot _ _ _ = pure () -- ignore plottng
      let onFrame = pure False -- never quit
      runEffect onPlot onFrame system

    True -> do
      Graphics.main system


prgOfNesFile :: NesFile -> PRG.ROM
prgOfNesFile NesFile{prgs} =
  case prgs of
    [prg2] -> prg2
    -- [prg1,prg2] -> (Just prg1, prg2)
    _  ->
      error $ "emu, unexpected number of prg: " <> show (length prgs)

makeSystem :: Config -> PRG.ROM -> Eff ()
makeSystem Config{stop_at,trace_cpu,init_pc} prg = do
  ppuState <- PPU.initState
  let ppuRegiserBus = PPU.makeRegisters ppuState
  bus <- makeCpuBus prg ppuRegiserBus -- including wram
  let ppu = PPU.ppu ppuState
  let
    cpuConfig = CPU.Config
      { trace = trace_cpu
      , stop_at = stop_at
      , init_pc = init_pc
      }
  let _cpu = CPU.cpu cpuConfig bus ppuState
  Parallel _cpu ppu
  --ppu

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
