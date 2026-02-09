module Top (main) where

--import Control.Monad (ap,liftM)
import Data.List (insertBy)
import Data.Ord (comparing)
import System.IO (stdout,hFlush,hPutStrLn)
import Text.Printf (printf)

main :: IO ()
main = do
  print "*emu-framework*"
  runEffect 22 system

----------------------------------------------------------------------
-- system to emulate

system :: Eff ()
system = Parallel cpu ppu

ppu :: Eff ()
ppu = loop 0
  where
    loop :: Int -> Eff ()
    loop i = do
      Log (show ("PPU, scanline=",i))
      Advance 7
      loop (i+1)

cpu :: Eff ()
cpu = do
  s <- mkCpuState
  loop 0 s
  where
    loop :: Int -> CpuState -> Eff ()
    loop i s = do
      op <- fetch s
      Log (show ("CPU",i,op))
      cycles <- dispatch s op
      Advance cycles
      loop (i+1) s

fetch :: CpuState -> Eff Op
fetch CpuState{} = do
  pure NOP

dispatch :: CpuState -> Op -> Eff Int
dispatch CpuState{} = \case
  NOP -> do
    -- do nothing
    pure 2

data Op = NOP deriving Show

data CpuState = CpuState

mkCpuState :: Eff CpuState
mkCpuState = pure CpuState

----------------------------------------------------------------------
-- effect

instance Functor Eff where fmap = undefined --liftM -- TODO: when are these needed?
instance Applicative Eff where pure = Ret; (<*>) = undefined --ap
instance Monad Eff where (>>=) = Bind

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  Log :: String -> Eff ()
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
