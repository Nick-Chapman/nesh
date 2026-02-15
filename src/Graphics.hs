
module Graphics (main) where

import CommandLine (Config)
import Control.Monad (when)
import Data.IORef (newIORef,readIORef,writeIORef)
import Foreign.C.Types (CInt)
import Framework (Eff(..),runEffect)
import Mapper (Mapper)
import PPU qualified (Graphics(..))
import SDL (V2(..),V4(..),($=))
import System (makeSystem)
import System.IO (hFlush,stdout,hPutStr)
import Text.Printf (printf)
import Types (RGB)
import qualified Data.Text as Text (pack)
import qualified SDL

main :: Config -> Mapper -> IO ()
main config mapper = do

  SDL.initializeAll

  let border = 5

  let screenW = 256 + 2*border
  let screenH = 240 + 2*border
  let sf = 2

  let
    scale :: CInt -> CInt
    scale x = sf * x

  let windowSize = V2 (scale screenW) (scale screenH)

  let winConfig = SDL.defaultWindow { SDL.windowInitialSize = windowSize
                                    , SDL.windowPosition = SDL.Absolute (SDL.P (SDL.V2 1000 100))
                                    }
  win <- SDL.createWindow (Text.pack "Dishonesty") $ winConfig
  renderer <- SDL.createRenderer win (-1) SDL.defaultRenderer

  SDL.rendererDrawColor renderer $= V4 100 100 100 255
  SDL.clear renderer
  SDL.present renderer

  lastTicks <- SDL.ticks >>= newIORef
  frameCounter <- newIORef (1::Int)

  let
    onPlot :: CInt -> CInt -> RGB -> Eff ()
    onPlot x0 y0 col = IO $ do
      let x = scale (x0 + border)
      let y = scale (y0 + border)
      SDL.rendererDrawColor renderer $= col
      let rect = SDL.Rectangle (SDL.P (V2 x y)) (V2 sf sf)
      SDL.fillRect renderer (Just rect)

    onFrame :: IO Bool
    onFrame = do
      SDL.present renderer

      n <- readIORef frameCounter
      writeIORef frameCounter (n+1)
      --putOut "."
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

  let graphics = PPU.Graphics
        { plot = onPlot
        , displayFrame = \_ -> do
            quit <- IO onFrame
            if quit then Halt else pure ()
        }
  let system = makeSystem config mapper graphics
  runEffect system

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
