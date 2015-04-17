module Main where

import Data.Monoid
import Control.Applicative
import Data.Composition
import Test.QuickCheck

-- some simple functions to work with
h :: Integer -> Integer -> Integer
h = (+) . (+ 3)
j :: Integer -> Integer
j = subtract 5

-- functions are applicative functors
k :: Integer -> Integer
k = h <$> j <*> j

-- functions are monads
i :: Integer -> Integer
i = j >>= \h' -> return $ h' - 20

o :: Integer -> Integer
o = do
    x   <- i
    x'  <- j
    x'' <- h x
    return $ x' + x''

-- composing is both a functor and monoidal with id
l :: [Integer] -> [Integer]
l = ([3] <>) <> ([5] <>)

n :: Integer
n = getSum . mconcat $ Sum <$> [1,2,3]

murf :: [String] -> [String]
murf xs = "murf" : xs

marf :: [String] -> [String]
marf xs = "marf" : xs

blurf :: [String] -> [String]
blurf xs = "blurf" : xs

b :: [String] -> [String]
b = do
    m  <- murf
    m' <- marf
    b' <- blurf
    return $ m <> m' <> b'

c :: [String] -> [String]
c = ((<>) .* (<>))
    <$> murf
    <*> marf
    <*> blurf

main :: IO ()
main = do
    print $ k 10 -- 13
    print $ i 10 -- -15
    print $ l [10] -- [3,10,5,10]
    print n -- 6
    quickCheck $ \x -> b x == c x

{-
instance Monad ((->) r) where
     return x = \_ -> x
     h >>= f = \w -> f (h w) w

could this be expressed as?

instance Monad ((->) r) where
     return = const
     h >>= f = (f . h) _
-}