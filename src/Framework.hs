module Framework
  ( Eff(..), runEffect
  , Ref(..), read, write
  ) where

--import Control.Monad (ap,liftM)
import Data.IORef (newIORef,readIORef,writeIORef)
import Data.List (insertBy)
import Data.Ord (comparing)
import Prelude hiding (read)
import System.IO (stdout,hFlush,hPutStrLn)
import Text.Printf (printf)
import Types (U8)

----------------------------------------------------------------------
-- Ref

data Ref a = Ref { onRead :: Eff a, onWrite :: a -> Eff () }

read :: Ref a -> Eff a
read Ref{onRead} = onRead

write :: Ref a -> a -> Eff ()
write Ref{onWrite} = onWrite

----------------------------------------------------------------------
-- effect

instance Functor Eff where fmap = undefined --liftM -- TODO: when are these needed?
instance Applicative Eff where pure = Ret; (<*>) = undefined --ap
instance Monad Eff where (>>=) = Bind

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  Log :: String -> Eff ()
  IO :: IO a -> Eff a
  DefineRegister :: a -> Eff (Ref a)
  DefineMemory :: Int -> Eff (Int -> Ref U8)
  Parallel :: Eff () -> Eff () -> Eff ()
  Advance :: Int -> Eff ()

runEffect :: Int -> Eff () -> IO ()
runEffect maxCycles eff0 = loop s0 eff0 k0
  where
    s0 = State { cycles = 0, jobs = [] }
    k0 () _ = error "effects should never end"

    logOut :: State -> String -> IO ()
    logOut State{cycles} message = do
      putOut $ printf "[%d] %s" cycles message

    loop :: State -> Eff a -> (a -> State -> IO ()) -> IO ()
    loop s@State{cycles=now} eff k = case eff of
      Ret a -> k a s
      Bind m f -> loop s m $ \a s -> loop s (f a) k
      Log message -> do
        logOut s message
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
        let
          f addr = do
            let onRead = error (show ("onRead",size,addr))
            let onWrite v = error (show ("onWrite",size,addr,v))
            Ref {onRead,onWrite}
        k f s

      Parallel m1 m2 -> do
        let j2 = Job { resumeTime = now, kunit = \s -> loop s m2 k0 }
        loop (pushJob s j2) m1 k
      Advance n -> do
        let jobMe = Job { resumeTime = now+n, kunit = k () }
        resumeNext (pushJob s jobMe)

    resumeNext :: State -> IO ()
    resumeNext s1 = do
      let State{jobs} = s1
      case jobs of
        [] -> error "resumeNext"
        firstJob:restJobs -> do
          let Job {resumeTime,kunit} = firstJob
          let s3 = s1 { cycles = resumeTime, jobs = restJobs }
          if timeToStop s3 then stop s3 else do
            kunit s3

    timeToStop :: State -> Bool
    timeToStop State{cycles} = cycles >= maxCycles

    stop state = do
      logOut state "Simulation ends"
      pure ()

putOut :: String -> IO ()
putOut s = do
  hPutStrLn stdout s
  hFlush stdout

data State = State
  { cycles :: Int
  , jobs :: [Job] -- ordered by resumeTime
  }

data Job = Job
  { resumeTime :: Int
  , kunit :: State -> IO ()
  }

pushJob :: State -> Job -> State
pushJob s@State{jobs} job =
  s { jobs = insertBy (comparing resumeTime) job jobs }
