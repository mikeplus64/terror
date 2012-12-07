{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Types where
import Data.Map.Strict                              (Map)
import Graphics.UI.GLFW                             (Key, MouseButton)
import Graphics.Rendering.OpenGL.Raw 
import Foreign

data Model = Model
    { modelBuffers :: !(Ptr GLuint)
    , modelFrames  :: !GLsizei
    } deriving Show

data Config = Config
    { resolution   :: (Int, Int)
    , quality      :: Int 
    , bloom        :: Bool
    , parallaxMaps :: Bool
    } deriving (Show, Read)

-- X : 0oX__
-- Y : 0o_Y_
-- Z : 0o__Z

data Subsystem
    = Events
    | Graphics
    | Sound
  deriving (Show, Eq)

data Event
    = KeyPress      !Key         !Bool
    | MouseMovement !Int         !Int
    | MouseButton   !MouseButton !Bool
    | Scroll        !Int
    | Pause
    | Quit
    | Death         !Subsystem
  deriving (Show, Eq)

data EngineSetup = EngineSetup
    { runKey        :: Key         -> IO ()
    , runMouse      :: MouseButton -> IO ()
    , graphicConfig :: Config
    }

