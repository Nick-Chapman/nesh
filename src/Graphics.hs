module Graphics (main) where

import CommandLine (Config)
import Control.Monad (when)
import Controller (Keys(..),makeKeys,seeKeys)
import Data.IORef (newIORef,readIORef,writeIORef)
import Foreign.C.Types (CInt)
import Framework (Eff(..),runEffect,write)
import Mapper (Mapper)
import PPU qualified (Graphics(..),Hack(..),makeHack)
import Prelude hiding (read)
import SDL (V2(..),V4(..),($=))
import System (makeSystem)
import Text.Printf (printf)
import Types (Colour)
import qualified Data.Text as Text (pack)
import qualified SDL

main :: Config -> Eff Mapper -> IO ()
main config mapperE = do

  SDL.initializeAll

  let border = 5

  let screenW = 256 + 2*border
  let screenH = 240 + 2*border
  let sf = 3

  let
    scale :: CInt -> CInt
    scale x = sf * x

  let windowSize = V2 (scale screenW) (scale screenH)

  let winConfig = SDL.defaultWindow { SDL.windowInitialSize = windowSize
                                    , SDL.windowPosition = SDL.Absolute (SDL.P (SDL.V2 1000 0))
                                    }
  win <- SDL.createWindow (Text.pack "nesh") $ winConfig
  renderer <- SDL.createRenderer win (-1) SDL.defaultRenderer

  let
    greyRenderer = do
      SDL.rendererDrawColor renderer $= V4 100 100 100 255
      SDL.clear renderer

  greyRenderer
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
    keys <- Controller.makeKeys
    hack <- PPU.makeHack
    let
      onPlot :: CInt -> CInt -> Colour -> Eff ()
      onPlot x0 y0 col = IO $ do
        let x = scale (x0 + border)
        let y = scale (y0 + border)
        SDL.rendererDrawColor renderer $= col
        let rect = SDL.Rectangle (SDL.P (V2 x y)) (V2 sf sf)
        SDL.fillRect renderer (Just rect)

      updateTitleBar frame = do
        let titleUpdateFrames = 10
        when (frame `mod` titleUpdateFrames == 0) $ do
          actualDuration <- IO $ durationSinceLastAsk
          let fpsAchieved = fromIntegral titleUpdateFrames * 1000 / actualDuration
          keysState <- Controller.seeKeys keys
          let title = printf "nesh keys=[%s] fps=[%.0g]" keysState fpsAchieved
          IO (SDL.windowTitle win $= Text.pack title) -- TODO: fixed width font

      displayFrame :: CInt -> Eff ()
      displayFrame frame = do
        IO $ SDL.present renderer
        IO $ greyRenderer
        events <- IO $ SDL.pollEvents
        mapM_ (processEvent keys hack) events
        updateTitleBar frame
        where

    let graphics = PPU.Graphics { plot = onPlot, displayFrame }
    makeSystem config mapperE hack keys graphics

  SDL.destroyRenderer renderer
  SDL.destroyWindow win
  SDL.quit

processEvent :: Keys -> PPU.Hack -> SDL.Event -> Eff ()
processEvent keys hack e = do
  case e of
    SDL.Event _t SDL.QuitEvent -> Halt
    SDL.Event _ (SDL.KeyboardEvent ke) -> do
      let code = SDL.keysymKeycode (SDL.keyboardEventKeysym ke)
      let motion = SDL.keyboardEventKeyMotion ke
      let drives = write (motion == SDL.Pressed)
      let quit = when (motion==SDL.Pressed) Halt
      case code of
        SDL.KeycodeEscape -> quit
        SDL.KeycodeQ -> quit

        -- Exploration/hacking
        --SDL.KeycodeTab -> drives invertBehind -- for lolz
        SDL.KeycodeTab -> drives noCPU
        SDL.KeycodeLShift -> drives noSprites
        SDL.KeycodeLCtrl -> drives noBG

        -- Controller-1 keys
        SDL.KeycodeReturn -> drives start
        SDL.KeycodeSpace -> drives select
        SDL.KeycodeA -> drives buttonA
        SDL.KeycodeD -> drives buttonB -- Note. This is on 'D'
        SDL.KeycodeLeft -> drives left
        SDL.KeycodeRight -> drives right
        SDL.KeycodeUp -> drives up
        SDL.KeycodeDown -> drives down

        _ -> pure ()
    _ -> pure ()

    where
      PPU.Hack{invertBehind=_,noCPU,noBG,noSprites} = hack
      Keys{start,select,buttonA,buttonB,left,right,up,down} = keys
