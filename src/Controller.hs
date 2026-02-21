module Controller
  ( Keys(..), makeKeys, seeKeys, State, initState, makeRegister
  ) where

import Control.Monad (when)
import Data.Bits (testBit)
import Framework (Eff(..),Ref(..),read,write)
import Prelude hiding (read)
import Types (U8)

data Keys = Keys
  { buttonA :: Ref Bool
  , buttonB:: Ref Bool -- on 'D'
  , select :: Ref Bool -- Space
  , start :: Ref Bool -- Enter
  , up :: Ref Bool
  , down :: Ref Bool
  , left :: Ref Bool
  , right :: Ref Bool
  }

makeKeys :: Eff Keys
makeKeys = do
  buttonA <- def; buttonB <- def; select <- def; start <- def;
  up <- def; down <- def; left <- def; right <- def;
  pure Keys{..}
    where def = DefineRegister False

seeKeys :: Keys -> Eff String
seeKeys keys = sequence [ do b <- read r; pure $ if b then c else '.'
                        | (r,c) <- labelledKeysInOrder keys ]

labelledKeysInOrder :: Keys -> [(Ref Bool,Char)]
labelledKeysInOrder keys =
  [ (buttonA,'A'),(buttonB,'B'),(select,'S'),(start,'E')
  , (up,'U'),(down,'D'),(left,'L'),(right,'R') ]
  where Keys{buttonA,buttonB,select,start,up,down,left,right} = keys

data State = State
  { keys :: Keys
  , strobe :: Ref Bool
  , scan :: Ref [(Bool,Char)]
  }

initState :: Keys -> Eff State
initState keys = do
  strobe <- DefineRegister False
  scan <- DefineRegister []
  pure State {..}

makeRegister :: State -> Ref U8
makeRegister State{keys,strobe,scan} = Ref {onRead,onWrite}
  where
    onWrite v = do
      when (v `testBit` 0) $ do
        write True strobe
        bools <- sequence [ do b <- read r; pure (b,c) | (r,c) <- labelledKeysInOrder keys ]
        write bools scan

    onRead = do
      read scan >>= \case
        [] -> pure 1
        (b,_tag):xs -> do
          --when b $ Log (show ("Controller:read",_tag))
          write xs scan
          pure (if b then 1 else 0)
