module TransformerFun where

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

fobots = return 3 :: IO Integer

-- hmmm, how to use lift?
--i :: Integer -> Integer -> MaybeT IO Integer
--i x y = return $ (x +) <$> fobots

glukkon :: IO ()
glukkon = do
    print $ runMaybeT g
    quickCheck $ \x y -> f x y == h x y