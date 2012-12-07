{-# LANGUAGE BangPatterns #-}
module Util where
import Control.Monad
import Control.Concurrent
import Data.List (delete)

infixr 0 <&&>

{-# INLINE while #-}
while :: IO Bool -> IO ()
while !x = go
  where
    go = do
        r <- x
        when r go

{-# INLINE (<&&>) #-}
-- | a lifted 'when'
(<&&>) :: IO Bool -> IO a -> IO ()
pred' <&&> x = do
    p <- pred' 
    when p (void x)

{-# INLINE waitFor #-}
-- | wait for each element of the list to be received in the chan
waitFor :: Eq a => Chan a -> [a] -> IO ()
waitFor m = go
  where
    go [] = return ()
    go xs = do
        r <- readChan m
        if r `elem` xs
          then go (delete r xs)
          else go xs


