module Start (main) where

import Bus (makeCpuBus)
import CHR qualified (ROM)
import CPU qualified (Config(..),mkState,cpu)
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
  let system = makeSystem config nesfile
  case sdl of
    False -> do
      let onPlot _ _ _ = pure () -- ignore plottng
      let onFrame = pure False -- never quit
      runEffect onPlot onFrame system
    True -> do
      Graphics.main system

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

makeSystem :: Config -> NesFile -> Eff ()
makeSystem Config{stop_at,trace_cpu,init_pc} nesfile = do
  let prg = prgOfNesFile nesfile
  let chr = chrOfNesFile nesfile
  ppuState <- PPU.initState chr
  let ppuRegiserBus = PPU.makeRegisters ppuState
  bus <- makeCpuBus prg ppuRegiserBus -- including wram
  cpuState <- CPU.mkState bus
  --let _peekCpuCyc = CPU.peekCYC cpuState
  let ppu = PPU.ppu ppuState
  let
    cpuConfig = CPU.Config
      { trace = trace_cpu
      , stop_at = stop_at
      , init_pc = init_pc
      }
  let cpu = CPU.cpu cpuConfig cpuState ppuState
  Parallel cpu ppu

prgOfNesFile :: NesFile -> PRG.ROM
prgOfNesFile NesFile{prgs} =
  case prgs of
    [prg2] -> prg2
    -- [prg1,prg2] -> (Just prg1, prg2)
    _  ->
      error $ "emu, unexpected number of prg: " <> show (length prgs)

chrOfNesFile :: NesFile -> CHR.ROM
chrOfNesFile NesFile{chrs} =
  case chrs of
    [chr] -> chr
    _  -> error "emu, unexpected number of chr"
