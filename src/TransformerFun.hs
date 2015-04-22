module TransformerFun where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Test.QuickCheck

g :: MaybeT [] Int
g = MaybeT [Just 3, Nothing, Just 3]

f, h :: Integer -> Integer -> Maybe Integer
f x y = liftM (+ x) (Just y)
h x y = (+ x) <$> (Just y)

glukkon :: IO ()
glukkon = do
    print $ runMaybeT g
    quickCheck $ \x y -> f x y == h x y