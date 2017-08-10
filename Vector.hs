{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Vector where

import Prelude hiding (head, tail, append, map, init, last, min, zipWith)

data Nat = Z | S Nat deriving (Show)

infixl 6 :+
infixl 7 :*
infixl 9 :~

type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z     :+ m = m
type instance (S n) :+ m = S (n :+ m)

type family   (n :: Nat) :* (m :: Nat) :: Nat
type instance Z     :* m = Z
type instance (S n) :* m = (n :* m) :+ m

type family   (n :: Nat) :~ (m :: Nat) :: Nat
type instance Z :~ Z         = Z
type instance Z :~(S _)      = Z
type instance (S _) :~ Z     = Z
type instance (S m) :~ (S n) = S (m :~ n)

data Vector (a :: *) (n :: Nat) where
  Nil  :: Vector a Z
  (:-) :: a -> Vector a n -> Vector a (S n)
infixr 5 :-

deriving instance Eq a => Eq (Vector a n)

instance Show a => Show (Vector a n) where
  showsPrec d = showsPrec d . toList

toList :: Vector a n -> [a]
toList Nil       = []
toList (x :- xs) = x : toList xs

-- FIXME
-- fromList :: Nat -> [a] -> Vector a n
-- NOTE: How to infer length of list?

head :: Vector a (S n) -> a
head (x :- _) = x

tail :: Vector a (S n) -> Vector a n
tail (_ :- xs) = xs

append :: Vector a n -> Vector a m -> Vector a (n :+ m)
append (x :- xs) ys = x :- append xs ys
append Nil ys       = ys

map :: (a -> b) -> Vector a n -> Vector b n
map _ Nil       = Nil
map f (x :- xs) = f x :- map f xs

uncons :: Vector a (S n) -> (a, Vector a n)
uncons (x :- xs) = (x, xs)

init :: Vector a (S n) -> Vector a n
init (x :- xs) = 
  case xs of
    Nil      -> Nil
    (_ :- _) -> x :- init xs

last :: Vector a n -> a
last (x :- Nil) = x
last (x :- xs)  = last xs

zipWithSame :: (a -> b -> c) -> Vector a n -> Vector b n -> Vector c n
zipWithSame _ Nil Nil             = Nil
zipWithSame f (a :- as) (b :- bs) = f a b :- zipWithSame f as bs

zipWith :: (a -> b -> c) -> Vector a n -> Vector b m -> Vector c (n :~ m)
zipWith _ Nil Nil             = Nil
zipWith _ Nil (_ :- _)        = Nil
zipWith _ (_ :- _)   Nil      = Nil
zipWith f (a :- as) (b :- bs) = f a b :- zipWith f as bs