module ArrowFun where

import Control.Arrow
import Data.Monoid

murf :: Arrow a => a b c -> a b' c' -> a (b, b') (c, c')
murf = (***)

buff :: (Int, Int) -> (Int, Int)
buff = (+ 3) *** (+ 4)

--               (y, z)
-- x  ┳→ (+ 3) ┛  │
--    ┖→ (+ 4) ───┙
chuff :: Integer -> (Integer, Integer)
chuff = (+ 3) &&& (+ 4)

glukkon :: IO ()
glukkon = do
    print $ "buff" <> show (buff (5, 5))
    print $ "chuff" <> show (chuff 10)