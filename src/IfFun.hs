module IfFun where

import Data.Composition
import Control.Concatenative
import Control.Applicative
import Control.Monad

if' :: Bool -> a -> a -> a
if' True then' _     = then'
if' _    _     else' = else'

-- interact with a condition with (->)
-- all functions passed with recieve the conidtion
ify :: (a -> Bool) -- if
    -> (a -> b)    -- then
    -> (a -> b)    -- else
    -> a -> b
-- ify comparision then' else' x =
--    if' (comparision x) (then' x) (else' x)
ify = (<*>) .* liftM2 if'

-- ify = ifte in Control.Concatenative

-- Monadic version of ify.
-- `if` is performed on the contents of the monad
-- the codomain of then and else must be instances of the monad
ifteM :: Monad m
    =>  (a -> Bool) -- if
    ->  (a -> m b)  -- then
    ->  (a -> m b)  -- else
    -> m a -> m b
ifteM = (=<<) .** ifte

ifteM' :: Monad m
    => (a -> Bool)
    -> (a -> b)
    -> (a -> b)
    -> m a -> m b
-- ifyM' = ((flip flip (return .) . ((.) .)) . (. (return .))) . ifyM
ifteM' c t e = ifteM c (return . t) (return . e)

data IF a = IF a a

runIF :: IF a -> Bool -> a
runIF (IF t _) True = t
runIF (IF _ e) _    = e
