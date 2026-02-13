module PPU
  ( State, initState, readPosition
  , makeRegisters
  , ppu
  ) where

import Control.Monad (when,forM_)
import Framework (Eff(..),Ref(..),read) --,write,update)
import Prelude hiding (read)
import Text.Printf (printf)
import Types (Addr,U8,RGB)
import Foreign.C.Types (CInt)
import SDL (V4(..))

ppu :: State -> Eff ()
ppu State{} = _v1 -- SELECT VERSION HERE

-- In total, we have 262 (1+240+21) lines y:[-1..260]
--   One pre-visible line, y:-1
--   240 visible lines, y:[0..239]
--   21 post-visible lines, y:[240..260]

-- BASELINE version. Just over 50fps
-- htop shows MEM% slowly increasing over time.
-- Over time, speed seems to slowly decrease down to 45fps.
-- And we see noticable GC pauses in the rendering

-- "Strict" language extension -- MEM% stops growing. Stable at 0.2%.
-- Opt level -O (= -O1) instead of -O2 increases speed to 73fps.

_v1 :: Eff ()
_v1 = loop 0
  where
    loop frame = do
      AdvancePPU 341
      forM_ [0..239] $ \y -> do
        forM_ [0..255] $ \x -> do
          let col = gradientCol frame x y
          Plot x y col
        AdvancePPU 341
      AdvancePPU (21 * 341)
      NewFrame
      loop (frame+1)

-- v2: Consolidate AdvancePPU calls.
-- Would have expected this to be quicker. BUT IT IS NOT!
-- In fact nearly half the speed. Very confused. Why is this??
_v2 :: Eff ()
_v2 = loop 0
  where
    loop frame = do
      forM_ [0..239] $ \y -> do
        forM_ [0..255] $ \x -> do
          let col = gradientCol frame x y
          Plot x y col
      AdvancePPU (262 * 341)
      NewFrame
      loop (frame+1)

-- v3: Never advance. So CPU, never gets a chance to run. Much quicker 130fps.
-- htop shows MEM% stable at around 0.2%
_v3 :: Eff ()
_v3 = loop 0
  where
    loop frame = do
      forM_ [0..239] $ \y -> do
        forM_ [0..255] $ \x -> do
          let col = gradientCol frame x y
          Plot x y col
      NewFrame
      loop (frame+1)

-- v4: AdvancePPU one cycle at a time, in the inner most loop
-- seems pretty similar to v1. htop shows slow MEM increase
_v4 :: Eff ()
_v4 = loop 0
  where
    loop frame = do
      forM_ [0..239] $ \y -> do
        forM_ [0..255] $ \x -> do
          let col = gradientCol frame x y
          Plot x y col
          AdvancePPU 1
      NewFrame
      loop (frame+1)

-- v5: AdvancePPU 20 cycles at a time.
-- chosen to be closish to the average advance expected from the CPU. (7 cycles * 3 ppu/ccp)
-- hmm. This will in effect cause the CPU to run 20x faster
-- so would expect a massive degration of rendering performance
-- Indeed. See about 10fps
_v5 :: Eff ()
_v5 = loop 0
  where
    loop frame = do
      forM_ [0..239] $ \y -> do
        forM_ [0..255] $ \x -> do
          let col = gradientCol frame x y
          Plot x y col
          AdvancePPU 20
      NewFrame
      loop (frame+1)



gradientCol :: CInt -> CInt -> CInt -> RGB
gradientCol frame x y = do
  let r = fromIntegral (y + frame)
  let g = 0
  let b = fromIntegral (x + frame)
  V4 r g b 255

----------------------------------------------------------------------
-- registers

type Bus = (Addr -> Ref U8)

makeRegisters :: State -> Bus
makeRegisters s = do
  let ppuCtrl = makeRegister s "PPUCTRL"
  let ppuStatus = makeRegister s "PPUSTATUS"
  \a -> do
    if
      | a == 0x2000 -> ppuCtrl
      | a == 0x2002 -> ppuStatus

      | otherwise ->
        error $ printf "PPU.makeRegisters: address = $%04X" a

makeRegister :: State -> String -> Ref U8
makeRegister s@State{} name = Ref {onRead,onWrite}
  where
    log = False
    onRead = do
      let v = 0
      when log $ ppuLog s $ printf "%s: read -> %02x" name v
      pure v

    onWrite v = do
      when log $ ppuLog s $ printf "%s: write %02x" name v
      pure ()

ppuLog :: State -> String -> Eff ()
ppuLog State{frame,x,y} message = do
  frame <- read frame
  x <- read x
  y <- read y
  Log $ printf "%d (%3d,%3d) : %s" frame x y message

----------------------------------------------------------------------
-- state

data State = State
  { frame :: Ref Int
  , y :: Ref Int
  , x :: Ref Int
  }

initState :: Eff State
initState = do
  frame <- DefineRegister 0
  y <- DefineRegister (-1)
  x <- DefineRegister 0
  pure State { frame, y, x }

readPosition :: State -> Eff (Int,Int)
readPosition State{x,y} = do
  x <- read x
  y <- read y
  pure (x,y)
