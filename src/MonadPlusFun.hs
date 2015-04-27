module MonadPlusFun where

import Control.Monad
import Control.Applicative
import Data.Monoid
import Test.QuickCheck

-- in the specific case, MonadPlus is the same as Monoid
h, g :: [Int] -> [Int] -> [Int]
h = mplus
g = (<>)

j :: Show a => [a] -> [a] -> [String]
j x y = show <$> y `mplus` x

-- mplus of Maybe just picks the one on the left, unless its Nothing
k, s :: Maybe Int
k = Just 3 `mplus` Just 6
s = Nothing `mplus` Just 3

-- Either is not a Monoid?
--q :: Either String Int
--q = Left "foo" `mplus` Right 3

glukkon :: IO ()
glukkon = do
    print $ show k <> " | " <> show s
    quickCheck $ \x y -> h x y == g x y