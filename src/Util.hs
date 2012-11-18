{-# LANGUAGE BangPatterns #-}
module Util where
import Control.Monad

infixr 0 <&&>

{-# INLINE while #-}
while :: IO Bool -> IO ()
while !x = go
  where
    go = do
        r <- x
        when r go

-- | a lifted "when"
{-# INLINE (<&&>) #-}
(<&&>) :: IO Bool -> IO a -> IO ()
pred' <&&> x = do
    p <- pred' 
    when p (void x)

