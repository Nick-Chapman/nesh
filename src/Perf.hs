module Perf (explorePerf) where

import Control.Monad (when)
import CommandLine (Config(..))
import Controller (makeKeys)
import Framework (Eff(..),runEffect,update,read,write)
import Mapper (Mapper)
import PPU qualified (Graphics(..),makeHack,Hack(..))
import System (makeSystem)
import Text.Printf (printf)
import Prelude hiding (read)
import Types (Colour)
import Timing (timed)

mergeColour :: Colour -> Colour -> Colour
mergeColour a b = a+b

explorePerf :: Config -> Eff Mapper -> IO ()
explorePerf config mapperE = do
  print "explore-perf"
  let
    system = do
      hack <- PPU.makeHack
      count <- DefineRegister (0::Int)
      smudge <- DefineRegister (0::Colour)
      let
        plot _ _ col = do
          update (+1) count
          update (mergeColour col) smudge
          pure ()

      let
        displayFrame n = do
          count <- read count
          smudge <- read smudge
          Print $ printf "frame=%s, #plots=%d, smudge=%s \n" (show n) count (show smudge)
          when (n==12) $ Halt -- should take 1/5s, 200ms

      let graphics = PPU.Graphics { plot, displayFrame }

      keys <- Controller.makeKeys
      let PPU.Hack{noCPU} = hack
      let _no = write True noCPU
      makeSystem config mapperE hack keys graphics

  time <- timed $ runEffect system
  print time
  printf "DONE\n"

