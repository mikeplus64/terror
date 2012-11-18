module Events where
import Control.Concurrent
import Types

handleEvents :: MVar World -> Chan Event -> IO ()
handleEvents = undefined

