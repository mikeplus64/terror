{-# LANGUAGE RecordWildCards, BangPatterns #-}
module Video where

import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW
import Control.Applicative
import Control.Monad
import qualified Data.Vector.Storable as S
import Foreign hiding (unsafePerformIO)
import System.IO.Unsafe (unsafePerformIO)

import Types

initializeVideo :: IO ()
initializeVideo = do
    glMatrixMode gl_PROJECTION
    glLoadIdentity

render :: IO ()
render = do
    glClear gl_COLOR_BUFFER_BIT
    glColor3f 1 1 1
    renderModel 0 sampleModel

vector2Model :: [S.Vector GLfloat] -> IO Model
vector2Model []      = error "vector2Model: cannot make model from empty vector list"
vector2Model vectors = do
    modelBufferObjects <- mallocArray len
    glGenBuffers modelFrames modelBufferObjects 
    forM_ (zip [0..len] vectors) $ \ (i,v) -> S.unsafeWith v $ \ vptr -> do
        buffer <- peekElemOff modelBufferObjects i
        glBindBuffer gl_ARRAY_BUFFER buffer
        glBufferData gl_ARRAY_BUFFER (fromIntegral size) vptr gl_DRAW_BUFFER
        glVertexAttribPointer buffer (fromIntegral size) gl_FLOAT 0 0 nullPtr
        glBindBuffer gl_ARRAY_BUFFER 0
    return Model{..}
  where
    len         = length vectors
    modelFrames = fromIntegral len 
    size        = sizeOf (undefined::GLfloat) * len

mkModel :: [GLfloat] -> IO Model
mkModel verts = do
    buffers <- malloc
    glGenBuffers 1 buffers
    buffer1 <- peek buffers
    glBindBuffer gl_ARRAY_BUFFER buffer1
    withArray verts $ \ vptr -> glBufferData gl_ARRAY_BUFFER (fromIntegral $ sizeOf (undefined::GLfloat) * length verts) vptr gl_DRAW_BUFFER
    return $! Model buffers 1

deleteModel :: Model -> IO ()
deleteModel Model{..} = do
    glDeleteBuffers modelFrames modelBufferObjects
    free modelBufferObjects

renderModel :: Int -> Model -> IO ()
renderModel frame Model{..} = peekElemOff modelBufferObjects frame >>= glDrawBuffer

sampleModel :: Model
sampleModel = unsafePerformIO $ vector2Model [S.fromList [0..10]]

