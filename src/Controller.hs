module Controller
  ( initKeys, Keys(..), seeKeys
  ) where

import Prelude hiding (read)
import Framework (Eff(..),Ref,read)

data Keys = Keys
  { start :: Ref Bool -- Enter
  , select :: Ref Bool -- Space
  , buttonA :: Ref Bool
  , buttonB:: Ref Bool -- on 'D'
  -- Arrow keys:
  , left :: Ref Bool
  , right :: Ref Bool
  , up :: Ref Bool
  , down :: Ref Bool
  }

initKeys :: Eff Keys
initKeys = do
  start <- DefineRegister False
  select <- DefineRegister False
  buttonA <- DefineRegister False
  buttonB <- DefineRegister False
  left <- DefineRegister False
  right <- DefineRegister False
  up <- DefineRegister False
  down <- DefineRegister False
  pure Keys{..}

seeKeys :: Keys -> Eff String
seeKeys keys =
  sequence [ do b <- read r; pure $ if b then c else '.' | (r,c) <- all ]
  where
    all = [ (start,'E') , (select,'S') , (buttonA,'A') , (buttonB,'B')
          , (left,'L') , (right,'R') , (up,'U') , (down,'D')
          ]
    Keys{start,select,buttonA,buttonB,left,right,up,down} = keys
