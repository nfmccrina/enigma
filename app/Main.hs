module Main where

import Lib
import Control.Monad.State

main :: IO ()
main = someFunc

-- main :: IO ()
-- main = runStateT bla 5 >> return ()
--main = runStateT code [1..] >> return ()
--
-- layer an infinite list of uniques over the IO monad
--

bla :: StateT Integer IO ()
bla = do
    x <- get
    io2 $ print x
    put 9
    y <- get
    io2 $ print y

code :: StateT [Integer] IO ()
code = do
    x <- pop
    io $ print x
    y <- pop
    io $ print y
    return ()
 
--
-- pop the next unique off the stack
--
pop :: StateT [Integer] IO Integer
pop = do
    (x:xs) <- get
    put xs
    return x
 
io :: IO a -> StateT [Integer] IO a
io = liftIO

io2 :: IO a -> StateT Integer IO a
io2 = liftIO