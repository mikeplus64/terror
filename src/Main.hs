{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL (viewport, Position(..), Size(..))
import Control.Concurrent
import System.Environment (getArgs)
import Control.Applicative
import Control.Monad

import Data.IORef

import Data.Foldable (for_)

import qualified Data.Map.Strict as M

import Video
import Events
import Types
import Util

main :: IO ()
main = do
    config      <- read <$> readFile "config"
    eventStream <- newChan
    continue    <- newIORef True
    initialize
    openWindow defaultDisplayOptions

    initializeVideo
    setupEvents eventStream continue

    while $ do
        swapBuffers
        delta <- getTime
        sleep (fps - delta)
        resetTime
        render
        readIORef continue

    writeChan eventStream (Death Graphics)

  where fps = 1/60

