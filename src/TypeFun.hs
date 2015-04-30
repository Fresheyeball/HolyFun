{-# LANGUAGE TypeFamilies #-}
module TypeFun where

type family Foo a

-- what in the unholy fuck?
-- pretty sure this is evil
type instance Foo Int = String
type instance Foo String = Int


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