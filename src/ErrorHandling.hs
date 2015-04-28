module ErrorHandling where

import Data.Monoid
import Control.Monad.Trans
import Control.Monad.Trans.Error

data Fubar = Fubar String

instance Error Fubar where
    noMsg  = Fubar mempty
    strMsg = Fubar

fobots :: IO Int
fobots = return 4

if' :: Bool -> t -> t -> t
if' c t e = if c then t else e

dangerous :: ErrorT Fubar IO Int
dangerous = do x <- lift fobots
               if' (x < 5) (return x) (throwError . Fubar $ "less than 5!")