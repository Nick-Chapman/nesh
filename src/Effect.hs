
module Effect
  ( Eff,ioEff,halt,parallel,advancePPU,now
  , runEffect
  ) where

import Control.Monad (ap,liftM)
import Data.List (insertBy)
import Data.Ord (comparing)

halt :: Eff a
now :: Eff Int
advancePPU :: Int -> Eff ()
ioEff :: IO a -> Eff a
parallel :: Eff () -> Eff () -> Eff ()
ret :: a -> Eff a
bind :: Eff a -> (a -> Eff b) -> Eff b


instance Functor Eff where
  {-# INLINE fmap #-}
  fmap = liftM

instance Applicative Eff where
  {-# INLINE pure #-}
  pure = ret
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad Eff where
  {-# INLINE (>>=) #-}
  (>>=) = bind

----------------------------------------------------------------------

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

----------------------------------------------------------------------

runEffect :: Eff () -> IO ()
runEffect m = loop m s0 k0
  where
    s0 = State { cycles = 0, jobs = [] }
    k0 () _ = error "effects should never end"

newtype Eff a = Eff { loop :: State -> (a -> State -> IO ()) -> IO () }

{-# INLINE ret #-}
{-# INLINE bind #-}
{-# INLINE ioEff #-}

ret a = Eff $ \s k -> k a s
bind m f = Eff $ \s k -> loop m s (\a s -> loop (f a) s k)
ioEff io = Eff $ \s k -> do x <- io; k x s
halt = Eff $ \_ _ -> pure ()
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


----------------------------------------------------------------------
{-
runEffect :: Eff () -> IO ()
runEffect eff0 = loop eff0 s0 k0
  where
    s0 = State { cycles = 0, jobs = [] }
    k0 () _ = error "effects should never end"

    loop :: Eff a -> State -> (a -> State -> IO ()) -> IO ()
    loop eff s k = case eff of
      Ret a -> k a s
      Bind m f -> loop m s $ \a s -> loop (f a) s k
      Halt -> pure ()
      IO io -> do x <- io; k x s
      Cycles -> do
        let State{cycles} = s
        k cycles s
      Parallel m1 m2 -> do
        let State{cycles=now} = s
        let j2 = Job { resumeTime = now, kunit = \() s -> loop m2 s k0 }
        loop m1 (pushJob s j2) k
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

ret = Ret
bind = Bind
halt = Halt
now = Cycles
advancePPU = AdvancePPU
ioEff = IO
parallel = Parallel

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  Halt :: Eff a
  IO :: IO a -> Eff a
  Cycles :: Eff Int
  Parallel :: Eff () -> Eff () -> Eff ()
  AdvancePPU :: Int -> Eff () -- we synchronise everything on PPU ticks

-}
