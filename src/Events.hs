{-# LANGUAGE BangPatterns #-}
module Events where
import Control.Concurrent
import Data.IORef
import Graphics.UI.GLFW
import Types
import Util

setupEvents :: Chan Event -> IORef Bool -> IO ThreadId
setupEvents chan continue = do
    setWindowCloseCallback $ do
        writeChan  chan Quit
        writeIORef continue False
        waitFor chan [Death Events, Death Graphics]
        terminate
        closeWindow
        return True

    setMousePositionCallback (\x y -> writeChan chan (MouseMovement x y))
    setMouseWheelCallback    (writeChan chan . Scroll)
    setMouseButtonCallback   (\k d -> writeChan chan (MouseButton k d))

    setKeyCallback $ \k d -> writeChan chan $! case k of
        KeyEsc -> Quit
        KeyF1  -> Pause
        _      -> KeyPress k d

    forkIO (handleEvents chan)
    
{-# INLINE handleEvents #-}
handleEvents :: Chan Event -> IO ()
handleEvents chan = go
  where
    go = do
        event <- readChan chan
        case event of Quit -> writeChan chan (Death Events)
                      _    -> print event >> go

