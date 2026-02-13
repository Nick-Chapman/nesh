
module Graphics (main) where

import Data.IORef (newIORef,readIORef,writeIORef)

import Framework (Eff,runEffect)
import Types --(U8,RGB(..))
import System.IO (hFlush,stdout,hPutStr)
import qualified Data.Text as Text (pack)
import Foreign.C.Types (CInt)
import Control.Monad (when)
import SDL (V2(..),V4(..),($=))
--import SDL.Video
import Text.Printf (printf)
import qualified SDL

main :: Eff () -> IO ()
main system = do

  SDL.initializeAll

  let border = 5

  let screenW = 256 + 2*border
  let screenH = 240 + 2*border
  let sf = 2

  let
    scale :: CInt -> CInt
    scale x = sf * x

  let windowSize = V2 (scale screenW) (scale screenH)

  let winConfig = SDL.defaultWindow { SDL.windowInitialSize = windowSize }
  win <- SDL.createWindow (Text.pack "Dishonesty") $ winConfig
  renderer <- SDL.createRenderer win (-1) SDL.defaultRenderer
--    { SDL.rendererType = SDL.AcceleratedRenderer }
--    { SDL.rendererType = SDL.UnacceleratedRenderer }
--    { SDL.rendererType = SDL.SoftwareRenderer }
--    { SDL.rendererType = SDL.AcceleratedVSyncRenderer }


  SDL.rendererDrawColor renderer $= V4 100 100 100 255
  SDL.clear renderer
  SDL.present renderer

  lastTicks <- SDL.ticks >>= newIORef
  frameCounter <- newIORef (1::Int)

  let
    onPlot :: CInt -> CInt -> RGB -> IO ()
    onPlot x0 y0 col = do
      let x :: CInt = scale (x0 + border)
      let y = scale (y0 + border)
      --let RGB{r,g,b} = rgb
      --let c :: V4 U8 = V4 r g b 255
      SDL.rendererDrawColor renderer $= col
      let rect = SDL.Rectangle (SDL.P (V2 x y)) (V2 sf sf)
      SDL.fillRect renderer (Just rect)

    onFrame :: [(CInt,CInt,RGB)] -> IO Bool
    onFrame _xs = do
      sequence_ [ onPlot x y c | (x,y,c) <- _xs ]
      SDL.present renderer

      n <- readIORef frameCounter
      writeIORef frameCounter (n+1)
      putOut "."
      -- every 60 frames, display fps achieved
      when (n `mod` 60 == 0) $ do
        t1 <- readIORef lastTicks
        t2 <- SDL.ticks
        writeIORef lastTicks t2
        let actualDuration :: Double = fromIntegral $ max (t2 - t1) 1
        let fpsAchieved = 60 * 1000 / actualDuration
        putOut $ printf "[%.0g]" fpsAchieved

      events <- SDL.pollEvents
      let quit = any isQuitEvent events
      pure quit

  runEffect onPlot onFrame system

  putOut "\n"
  SDL.destroyRenderer renderer
  SDL.destroyWindow win
  SDL.quit

putOut :: String -> IO ()
putOut s = do
  hPutStr stdout s
  hFlush stdout

isQuitEvent :: SDL.Event -> Bool
isQuitEvent = \case
  SDL.Event _t SDL.QuitEvent -> True
  SDL.Event _ (SDL.KeyboardEvent ke) -> do
    let code = SDL.keysymKeycode (SDL.keyboardEventKeysym ke)
    let motion = SDL.keyboardEventKeyMotion ke
    case (code,motion) of
      (SDL.KeycodeEscape,SDL.Pressed) -> True
      (SDL.KeycodeQ,SDL.Pressed) -> True
      _ -> False
  _ -> False
