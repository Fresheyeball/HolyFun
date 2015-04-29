module ErrorHandling where

import Data.Monoid
import Control.Monad.Trans
import Control.Monad.Trans.Error
import If

data Fubar = Fubar String

instance Error Fubar where
    noMsg  = Fubar mempty
    strMsg = Fubar

fobots :: IO Int
fobots = return 4

dangerous :: ErrorT Fubar IO Int
dangerous = lift fobots >>=
    ify (< 5) return (const . throwError . Fubar $ "less than 5!")