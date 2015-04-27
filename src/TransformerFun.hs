module TransformerFun where

import Data.Composition
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Test.QuickCheck

g :: MaybeT [] Int
g = MaybeT [Just 3, Nothing, Just 3]

f, h :: Int -> Int -> Maybe Int
f x y = liftM (+ x) (Just y)
h x y = (+ x) <$> (Just y)

fobots :: IO Int
fobots = return 3 :: IO Int

-- hmmm, how to use lift?
-- why is liftM generalized but lift is not?
--i :: Integer -> Integer -> MaybeT IO Integer
--i x y = return $ (x +) <$> fobots

-- ok this I like
u, i, j, k, l, n, z, zz :: MaybeT IO Int -> MaybeT IO Int

u x = x >>= (\i' -> if i' < 5
                    then return $ i' + 3
                    else mzero)

i x = (>>=) x $ \i' ->
         if i' < 5
         then return $ i' + 3
         else mzero

j = (>>= f')
    where f' x | x < 5 = return $ x + 3
               | otherwise = mzero

n x = do x' <- x
         if x' < 5 then return $ x' + 3 else mzero

-- This one seems to have the clearest intent
-- the do notation is hard to mistake from some curried fancyness
-- with (>>=). If statement broken to multiple lines gives the logic fork
-- appropriate weight.
k x = do x' <- x
         if x' < 5
         then return $ x' + 3
         else mzero

l = (=<<) $ \x' -> if x' < 5 then return $ x' + 3 else mzero

z = (>>= (\x' -> if x' < 5 then return $ x' + 3 else mzero))

zz = ifyM (< 5) (return . (+ 3)) (const mzero)


-- interact with a condition with (->)
-- all functions passed with recieve the conidtion
ify :: (a -> Bool) -- if
    -> (a -> b)    -- then
    -> (a -> b)    -- else
    -> a -> b
ify comparision then' else' x =
    if comparision x
    then then' x
    else else' x

-- Monadic version of ify.
-- `if` is performed on the contents of the monad
-- the codomain of then and else must be instances of the monad
ifyM :: Monad m
    =>  (a -> Bool) -- if
    ->  (a -> m b)  -- then
    ->  (a -> m b)  -- else
    -> m a -> m b
ifyM = (=<<) .** ify

-- lift is really just, take this monad and make it match the transformed version
-- so long as its the underlying monad. In this case an `IO Int` is being used inside
-- a `MaybeT IO Int` so lift is `IO Int -> MaybeT IO Int`. `m a -> t m a`
-- this means that all MonadTrans can take any arbitrary Monad and transform it.
-- much like all Monads can be `return`ed.
gent :: MaybeT IO Int
gent = do
    x <- lift fobots
    return $ 2 + x

glukkon :: IO ()
glukkon = do
    print $ runMaybeT g
    quickCheck $ \x y -> f x y == h x y