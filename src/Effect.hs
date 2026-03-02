
module Effect
  ( Eff,ioEff,halt,parallel,advancePPU,now
  , runEffect
  ) where

import Control.Monad (ap,liftM)
import Data.List (insertBy)
import Data.Ord (comparing)

{-# INLINE ret #-}
{-# INLINE bind #-}
{-# INLINE ioEff #-}

ret :: a -> Eff a
bind :: Eff a -> (a -> Eff b) -> Eff b
halt :: Eff a
ioEff :: IO a -> Eff a
now :: Eff Int
advancePPU :: Int -> Eff ()
parallel :: Eff () -> Eff () -> Eff ()

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = ret; (<*>) = ap
instance Monad Eff where (>>=) = bind

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

runEffect :: Eff () -> IO ()
runEffect m = loop m s0 k0
  where
    s0 = State { cycles = 0, jobs = [] }
    k0 () _ = error "effects should never end"

newtype Eff a = Eff { loop :: State -> (a -> State -> IO ()) -> IO () }

ret a = Eff $ \s k -> k a s
bind m f = Eff $ \s k -> loop m s (\a s -> loop (f a) s k)
halt = Eff $ \_ _ -> pure ()
ioEff io = Eff $ \s k -> do x <- io; k x s
now = Eff $ \s@State{cycles} k -> k cycles s

parallel m1 m2 = Eff $ \s k -> do
  let State{cycles=now} = s
  let j2 = Job { resumeTime = now, kunit = \() s -> loop m2 s k0 }
  loop m1 (pushJob s j2) k
    where k0 () _ = error "effects should never end"

advancePPU n = Eff $ \s k -> do
  let State{cycles=now} = s
  let now_n = now+n
  let jobMe = Job { resumeTime = now_n, kunit = k }
  resumeNext (pushJob s jobMe)
  where
    resumeNext :: State -> IO ()
    resumeNext s1 = do
      let State{jobs} = s1
      case jobs of
        [] -> error "resumeNext"
        firstJob:restJobs -> do
          let Job {resumeTime,kunit} = firstJob
          let s2 = s1 { cycles = resumeTime, jobs = restJobs }
          kunit () s2
