{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import Graphics.UI.GLFW
import Control.Concurrent
import System.Environment (getArgs)
import Control.Applicative
import Control.Monad

import Video
import Events
import Types
import Util

main :: IO ()
main = do
    (width:height:_) <- map read <$> getArgs
    initialize
    openWindow defaultDisplayOptions
    setWindowTitle "Terragenesis"
    setWindowDimensions width height

    eventStream <- newChan
    world       <- newMVar (undefined :: World)
    mainThread  <- myThreadId

    setKeyCallback           $ \key action -> undefined
    setMouseButtonCallback   $ \key action -> undefined
    setMousePositionCallback $ \key action -> undefined

    forkIO $ do
        handleEvents world eventStream
        killThread mainThread -- DIE DIE DIE

    forever $ do
        resetTime
        render
        delta <- getTime
        sleep (fps - delta)
  where fps = 1/60

