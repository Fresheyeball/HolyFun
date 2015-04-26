module CompositionFun where

import Data.Composition
import Data.Functor.Compose
import Data.Monoid
import Test.QuickCheck

comp2 ::           (c -> d)
      -> (a -> b -> c)
      ->  a -> b ->      d
comp2 = (.:)

f, g, h :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]

f a b c d = a <> b <> c <> d
g = (<>) .: (<>) .: (<>)

-- lisp style syntax
h = (comp2
        (comp2 mappend mappend)
             mappend)

glukkon :: IO ()
glukkon = quickCheck
        $ \a b c d -> f a b c d == g a b c d