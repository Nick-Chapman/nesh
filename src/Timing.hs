
module Timing (timed,Nanos) where

import GHC.Int (Int64)
import System.Clock (TimeSpec(..),getTime,Clock(Monotonic))
import Text.Printf (printf)

timed :: IO () -> IO Nanos
timed io = do
  before <- getTime Monotonic
  io
  after <- getTime Monotonic
  let TimeSpec{sec,nsec} = after - before
  pure $ Nanos (gig * sec + nsec)

newtype Nanos = Nanos Int64 deriving (Eq,Ord,Num)

instance Show Nanos where
  show (Nanos i) = printf "%.03fs" dub
    where dub :: Double = fromIntegral i / fromIntegral gig

gig :: Int64
gig = 1_000_000_000
