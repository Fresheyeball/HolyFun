module TransformerFun where

import Data.Composition
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Test.QuickCheck

g :: MaybeT [] Int
g = MaybeT [Just 3, Nothing, Just 3]

f, h :: Integer -> Integer -> Maybe Integer
f x y = liftM (+ x) (Just y)
h x y = (+ x) <$> (Just y)

fobots :: IO Integer
fobots = return 3 :: IO Integer

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

j = (>>= f)
    where f x | x < 5 = return $ x + 3
              | otherwise = mzero

n x = do x' <- x
         if x' < 5 then return $ x' + 3 else mzero

k x = do x' <- x
         if x' < 5
         then return $ x' + 3
         else mzero

l = flip (>>=) $ \x' -> if x' < 5 then return $ x' + 3 else mzero

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
    => (a -> Bool) -- if
    -> (a -> m b)  -- then
    -> (a -> m b)  -- else
    -> m a -> m b
ifyM = (=<<) .** ify

glukkon :: IO ()
glukkon = do
    print $ runMaybeT g
    quickCheck $ \x y -> f x y == h x y
