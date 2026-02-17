module Framework
  ( Eff(..), runEffect
  , Ref(..), read, write, update, dummyRef, dummyRef_quiet
  , Bus
  ) where

import Control.Monad (when,ap,liftM)
import Data.IORef (newIORef,readIORef,writeIORef)
import Data.List (insertBy)
import Data.Ord (comparing)
import GHC.IOArray (IOArray,newIOArray,writeIOArray,readIOArray)
import Prelude hiding (read)
import System.IO (stdout,hFlush,hPutStr)
import Text.Printf (printf)
import Types (U8,Addr)

----------------------------------------------------------------------
-- Ref

data Ref a = Ref { onRead :: Eff a, onWrite :: a -> Eff () }

read :: Ref a -> Eff a
read Ref{onRead} = onRead

write :: a -> Ref a -> Eff ()
write v Ref{onWrite} = onWrite v

update :: (a -> a) -> Ref a -> Eff ()
update f r = do
  v <- read r
  write (f v) r

type Bus = (Addr -> Eff (Ref U8))

dummyRef_maybeLog :: Bool -> String -> Addr -> Ref U8
dummyRef_maybeLog doLog tag a =
  Ref { onRead = do
          -- TODO: make these Errors to stop emulation
          when doLog $ Log (printf "TODO (%s): read: %04x" tag a)
          pure 0
      , onWrite = \v -> do
          when doLog $ Log (printf "TODO (%s): write: %04x = %02x" tag a v)
          pure ()
      }

dummyRef :: String -> Addr -> Ref U8
dummyRef = dummyRef_maybeLog True

dummyRef_quiet :: String -> Addr -> Ref U8
dummyRef_quiet = dummyRef_maybeLog False

----------------------------------------------------------------------
-- effect

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = Ret; (<*>) = ap
instance Monad Eff where (>>=) = Bind

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  Halt :: Eff ()
  IO :: IO a -> Eff a

  Print :: String -> Eff () -- raw flushed print
  Log :: String -> Eff () -- flushed/print showing #cycles & adding NL
  Error :: String -> Eff a -- print and stop

  DefineRegister :: a -> Eff (Ref a)
  DefineMemory :: Int -> Eff (Int -> Ref U8)

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

      Print message -> do
        putOut message
        k () s

      Log message -> do
        let State {cycles} = s
        putOut (printf "%6d: %s\n" cycles message)
        k () s

      Error message -> do
        putOut (printf "ERROR: %s\n" message)
        pure ()

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

      Cycles -> do
        let State{cycles=now} = s
        k now s

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
