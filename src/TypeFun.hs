{-# LANGUAGE TypeFamilies #-}
module TypeFun where

type family Foo a

-- what in the unholy fuck?
-- pretty sure this is evil
type instance Foo Int = String