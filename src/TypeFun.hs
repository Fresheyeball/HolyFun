{-# LANGUAGE TypeFamilies #-}
module TypeFun where

type family Foo a

-- what in the unholy fuck?
-- pretty sure this is evil
type instance Foo Int = String
type instance Foo String = Int

type family Bar

-- it just needs to be of kind *?
type instance Bar = Int
-- below is not ok because if overlaps
-- type instance Bar = String

-- not ok?
{-
foozle :: a -> Foo a
foozle _ = "what"
foozle _ = 3
-}

-- psycho, just psycho
foozle :: Int -> Foo Int
foozle _ = "what"

boozle :: Int -> Foo String
boozle = id