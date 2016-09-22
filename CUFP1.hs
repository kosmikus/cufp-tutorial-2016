{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
module CUFP1 where

import GHC.Exts (Constraint)

data YesNo = Yes | No

data Expr = Lit Int | Add Expr Expr

{-
class Generic a where
  type Rep a

  from :: a -> Rep a
  to :: Rep a -> a

geq :: Rep a -> Rep a -> Bool

eq :: (Generic a) => a -> a -> Bool
eq x y = geq (from x) (from y)
-}


data List a =
    LNil
  | LCons a (List a)

data Nat = Z | S Nat

-- Nat is a kind
-- Z :: Nat
-- S :: Nat -> Nat

data Vec (n :: Nat) (a :: *) where
  VNil :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

data HList (ts :: [*]) where
  HNil :: HList '[]
  HCons :: t -> HList ts -> HList (t ': ts)

data NP (f :: * -> *) (ts :: [*]) where
  Nil  :: NP f '[]
  (:*) :: f t -> NP f ts -> NP f (t ': ts)

deriving instance All Show ts => Show (NP I ts)
deriving instance Show a => Show (NP (K a) ts)
deriving instance All Show ts => Show (NP Maybe ts)

-- deriving instance Show (NP I '[])

-- All :: (* -> Constraint) -> [*] -> Constraint
type family All (c :: * -> Constraint) (ts :: [*]) :: Constraint where
  All c '[]       = ()
  All c (t ': ts) = (c t, All c ts)

infixr 5 :*

data I a = I a
  deriving Show

unI :: I a -> a
unI (I x) = x

data K a b = K a
  deriving Show

example1 :: NP I '[Int, Bool, Char]
example1 = I 3 :* I False :* I 'x' :* Nil

example2 :: NP (K String) '[Int, Bool, Char]
example2 =
  K "foo" :* K "bar" :* K "baz" :* Nil

example3 :: NP Maybe '[Int, Bool, Char]
example3 =
  Just 3 :* Nothing :* Just 'x' :* Nil

hd :: NP f (x ': xs) -> f x
hd (x :* xs) = x

hmap_NP :: (forall x . s x -> t x) -> NP s xs -> NP t xs
hmap_NP f Nil       = Nil
hmap_NP f (x :* xs) = f x :* hmap_NP f xs

hcmap_NP ::
  All c xs => Proxy c -> (forall x . c x => s x -> t x) -> NP s xs -> NP t xs
hcmap_NP p f Nil       = Nil
hcmap_NP p f (x :* xs) = f x :* hcmap_NP p f xs

data Proxy (a :: k) = Proxy



-- [a] is a kind
-- []  :: [a]
-- (:) :: a -> [a] -> [a]

hcollapse_NP :: NP (K a) xs -> [a]
hcollapse_NP Nil         = []
hcollapse_NP (K x :* xs) = x : hcollapse_NP xs






