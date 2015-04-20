module CompositionFun where

import Data.Composition
import Data.Monoid
import Test.QuickCheck

f, g :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
f a b c d = a <> b <> c <> d
g = (<>) .: (<>) .: (<>)

glukkon :: IO ()
glukkon = quickCheck
        $ \a b c d -> f a b c d == g a b c d