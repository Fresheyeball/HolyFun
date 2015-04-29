module ErrorHandlingFun where

import Data.Monoid
import Control.Monad.Trans
import Control.Monad.Trans.Error
import Control.Concatenative

data Fubar = Fubar String

instance Error Fubar where
    noMsg  = Fubar mempty
    strMsg = Fubar

fobots :: IO Int
fobots = return 4

dangerous :: ErrorT Fubar IO Int
dangerous = lift fobots >>=
    ifte (< 5) return (const . throwError . Fubar $ "less than 5!")
