module Framework
  ( Eff(..), runEffect
  , Ref(..), read, write, update
  ) where

import Control.Monad (ap,liftM)
import Data.IORef (newIORef,readIORef,writeIORef)
import Data.List (insertBy)
import Data.Ord (comparing)
import GHC.IOArray (IOArray,newIOArray,writeIOArray,readIOArray)
import Prelude hiding (read)
import System.IO (stdout,hFlush,hPutStr)
import Types (U8)

----------------------------------------------------------------------
-- Ref

data Ref a = Ref { onRead :: Eff a, onWrite :: a -> Eff () }

read :: Ref a -> Eff a
read Ref{onRead} = onRead

write :: Ref a -> a -> Eff ()
write Ref{onWrite} = onWrite

update :: (a -> a) -> Ref a -> Eff ()
update f r = do
  v <- read r
  write r (f v)

----------------------------------------------------------------------
-- effect

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = Ret; (<*>) = ap
instance Monad Eff where (>>=) = Bind

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  Halt :: Eff ()
  Log :: String -> Eff ()
  IO :: IO a -> Eff a
  DefineRegister :: a -> Eff (Ref a)
  DefineMemory :: Int -> Eff (Int -> Ref U8)
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

      Log message -> do
        putOut message
        k () s

      IO io -> do
        x <- io
        k x s

      DefineRegister v -> do
        r <- newIORef v
        k Ref { onRead = IO (readIORef r)
              , onWrite = \v -> IO (writeIORef r v)
              } s

      DefineMemory size -> do
        mem :: IOArray Int U8 <- newIOArray (0,size - 1) 0
        let
          f :: Int -> Ref U8
          f addr = do
            let onRead = IO (readIOArray mem addr)
            let onWrite v = IO (writeIOArray mem addr v)
            Ref {onRead,onWrite} -- TODO: optimization(?) pre-build each Ref
        k f s

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

putOut :: String -> IO ()
putOut s = do
  hPutStr stdout s
  hFlush stdout

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
