
module Graphics (main) where

import CommandLine (Config)
import Control.Monad (when)
import Data.IORef (newIORef,readIORef,writeIORef)
import Foreign.C.Types (CInt)
import Framework (Eff(..),runEffect,update,read)
import Mapper (Mapper)
import PPU qualified (initMode,nextMode,Graphics(..))
import Prelude hiding (read)
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
  let
    durationSinceLastAsk :: IO Double
    durationSinceLastAsk = do
      t1 <- readIORef lastTicks
      t2 <-  SDL.ticks
      writeIORef lastTicks t2
      pure $ fromIntegral $ max (t2-t1) 1

  runEffect $ do
    mode <- DefineRegister PPU.initMode
    let
      onPlot :: CInt -> CInt -> RGB -> Eff ()
      onPlot x0 y0 col = IO $ do
        let x = scale (x0 + border)
        let y = scale (y0 + border)
        SDL.rendererDrawColor renderer $= col
        let rect = SDL.Rectangle (SDL.P (V2 x y)) (V2 sf sf)
        SDL.fillRect renderer (Just rect)

      displayFrame :: Int -> Eff ()
      displayFrame frame = do
        --Print $ ","
        let titleUpdateFrames = 10
        do
          when (frame `mod` titleUpdateFrames == 0) $ do
            actualDuration <- IO $ durationSinceLastAsk
            let fpsAchieved = fromIntegral titleUpdateFrames * 1000 / actualDuration
            mode <- read mode
            let title = printf "Dishonesty (%s) fps=[%.0g]" (show mode) fpsAchieved
            IO (SDL.windowTitle win $= Text.pack title) -- TODO: fixed width font
        IO $ SDL.present renderer
        events <- IO $ SDL.pollEvents
        let quit = any isQuitEvent events
        let tab = any isTabEvent events
        if quit then Halt else pure ()
        if tab then update PPU.nextMode mode else pure ()

    let graphics = PPU.Graphics { plot = onPlot, displayFrame }
    makeSystem config mapper mode graphics

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

isTabEvent :: SDL.Event -> Bool
isTabEvent = \case
  SDL.Event _ (SDL.KeyboardEvent ke) -> do
    let code = SDL.keysymKeycode (SDL.keyboardEventKeysym ke)
    let motion = SDL.keyboardEventKeyMotion ke
    case (code,motion) of
      (SDL.KeycodeTab,SDL.Pressed) -> True
      _ -> False
  _ -> False
