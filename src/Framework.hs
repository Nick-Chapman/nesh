module Framework
  ( Eff, runEffect
  , defineRegister,defineMemory,parallel,log,effError,effPrint,halt,advancePPU,ioEff

  , Ref(..), read, write, update, dummyRef, dummyRef_quiet
  , Bus
  ) where

import Control.Monad (when)
import Data.IORef (newIORef,readIORef,writeIORef)
--import GHC.IOArray (IOArray,newIOArray,writeIOArray,readIOArray)
import Prelude hiding (read,log)
import System.IO (stdout,hFlush,hPutStr)
import Text.Printf (printf)
import Types (U8,Addr)

import Effect (Eff,ioEff,now,halt,runEffect,parallel,advancePPU)

import Data.Array (Array,listArray,(!))


{-# INLINE defineRegister #-}
defineRegister :: a -> Eff (Ref a)
defineRegister v = ioEff $ do
  r <- newIORef v
  pure $ Ref { onRead = ioEff (readIORef r)
             , onWrite = \v -> ioEff (writeIORef r v)
             }

{-
{-# INLINE defineMemory #-}
defineMemory :: Int -> Eff (Int -> Ref U8)
defineMemory size = ioEff $ do
  mem :: IOArray Int U8 <- newIOArray (0,size - 1) 0
  pure $ \addr -> do
    let onRead = ioEff (readIOArray mem addr)
    let onWrite v = ioEff (writeIOArray mem addr v)
    Ref {onRead,onWrite} -- TODO: optimization(?) pre-build each Ref
-}

{-# INLINE defineMemory #-}
defineMemory :: Int -> Eff (Int -> Ref U8)
defineMemory size = do
  rs <- sequence (replicate size (defineRegister 0))
  let a :: Array Int (Ref U8) = listArray (0, size - 1) rs
  pure $ \i -> do
    a ! i

log :: String -> Eff ()
log message = do
  cycles <- now
  ioEff $ putOut (printf "%6d: %s\n" cycles message)

putOut :: String -> IO ()
putOut s = do
  hPutStr stdout s
  hFlush stdout

effPrint :: String -> Eff () -- raw flushed print
effPrint message = ioEff $ do
  putOut message

effError :: String -> Eff a
effError message = do
  ioEff $ putOut (printf "ERROR: %s\n" message)
  halt

----------------------------------------------------------------------
-- Ref

data Ref a = Ref { onRead :: Eff a, onWrite :: a -> Eff () }

read :: Ref a -> Eff a
read Ref{onRead} = onRead

{-# INLINE write #-}
write :: a -> Ref a -> Eff ()
write v Ref{onWrite} = onWrite v

{-# INLINE update #-}
update :: (a -> a) -> Ref a -> Eff ()
update f r = do
  v <- read r
  write (f v) r

type Bus = (Addr -> Eff (Ref U8))

dummyRef_maybeLog :: Bool -> String -> Addr -> Ref U8
dummyRef_maybeLog doLog tag a =
  Ref { onRead = do
          -- TODO: make these Errors to stop emulation
          when doLog $ log (printf "TODO (%s): read: %04x" tag a)
          pure 0
      , onWrite = \v -> do
          when doLog $ log (printf "TODO (%s): write: %04x = %02x" tag a v)
          pure ()
      }

dummyRef :: String -> Addr -> Ref U8
dummyRef = dummyRef_maybeLog True

dummyRef_quiet :: String -> Addr -> Ref U8
dummyRef_quiet = dummyRef_maybeLog False
