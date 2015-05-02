module ImperativeFun where

import Data.IORef

-- Monadic IO means its not possible
-- to have a top level IORef
x :: IO (IORef Int)
x = newIORef 3

changeIt :: Int -> IO ()
changeIt y = do
    -- therefore x' here is not the same IORef in glukkon
    x' <- x
    writeIORef x' y

-- imperative is NOT possible, horray!
glukkon :: IO ()
glukkon = do
    x' <- x
    y  <- readIORef x'
    print y
    -- implicit state changes are not possible
    _  <- changeIt 4
    y' <- readIORef x'
    print y'

-- this code yields
-- 3
-- 3
