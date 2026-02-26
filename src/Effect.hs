
module Effect
  ( Eff,ioEff,halt,parallel,advancePPU,now
  , runEffect
  ) where

import Control.Monad (ap,liftM)
import Data.List (insertBy)
import Data.Ord (comparing)

halt :: Eff a
halt = Halt

now :: Eff Int
now = Cycles

advancePPU :: Int -> Eff ()
advancePPU = AdvancePPU

ioEff :: IO a -> Eff a
ioEff = IO

parallel :: Eff () -> Eff () -> Eff ()
parallel = Parallel

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = Ret; (<*>) = ap
instance Monad Eff where (>>=) = Bind

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  Halt :: Eff a
  IO :: IO a -> Eff a
  Cycles :: Eff Int
  Parallel :: Eff () -> Eff () -> Eff ()
  AdvancePPU :: Int -> Eff () -- we synchronise everything on PPU ticks

runEffect :: Eff () -> IO ()
runEffect eff0 = loop s0 eff0 k0
  where
    s0 = State { cycles = 0, jobs = [] }
    k0 () _ = error "effects should never end"

    loop :: State -> Eff a -> (a -> State -> IO ()) -> IO ()
    loop s eff k = case eff of
      Ret a -> k a s
      Bind m f -> loop s m $ \a s -> loop s (f a) k
      Halt -> pure ()
      IO io -> do x <- io; k x s
      Cycles -> do
        let State{cycles} = s
        k cycles s
      Parallel m1 m2 -> do
        let State{cycles=now} = s
        let j2 = Job { resumeTime = now, kunit = \() s -> loop s m2 k0 }
        loop (pushJob s j2) m1 k
      AdvancePPU n -> do
        let State{cycles=now} = s
        let now_n = now+n
        let jobMe = Job { resumeTime = now_n, kunit = k }
        resumeNext (pushJob s jobMe)

    resumeNext :: State -> IO ()
    resumeNext s1 = do
      let State{jobs} = s1
      case jobs of
        [] -> error "resumeNext"
        firstJob:restJobs -> do
          let Job {resumeTime,kunit} = firstJob
          let s2 = s1 { cycles = resumeTime, jobs = restJobs }
          kunit () s2

data State = State
  { cycles :: Int
  , jobs :: [Job] -- ordered by resumeTime
  }

data Job = Job
  { resumeTime :: Int
  , kunit :: () -> State -> IO ()
  }

pushJob :: State -> Job -> State
pushJob s@State{jobs} job =
  s { jobs = insertBy (comparing resumeTime) job jobs }

