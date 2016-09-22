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

data Nat = NatZ | NatS Nat

-- Nat is a kind
-- Z :: Nat
-- S :: Nat -> Nat

data Vec (n :: Nat) (a :: *) where
  VNil :: Vec NatZ a
  VCons :: a -> Vec n a -> Vec (NatS n) a

data HList (ts :: [*]) where
  HNil :: HList '[]
  HCons :: t -> HList ts -> HList (t ': ts)

data NP (f :: k -> *) (ts :: [k]) where
  Nil  :: NP f '[]
  (:*) :: f t -> NP f ts -> NP f (t ': ts)

deriving instance All Show ts => Show (NP I ts)
deriving instance Show a => Show (NP (K a) ts)
deriving instance All Show ts => Show (NP Maybe ts)

-- deriving instance Show (NP I '[])

-- All :: (k -> Constraint) -> [k] -> Constraint
type family All (c :: k -> Constraint) (ts :: [k]) :: Constraint where
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

data NS (f :: k -> *) (ts :: [k]) where
  Z :: f t -> NS f (t ': ts)
  S :: NS f ts -> NS f (t ': ts)

-- hmap_NS :: (forall x . s x -> t x) -> NS s xs -> NS t xs
-- hmap_SOP :: (forall x . s x -> t x) -> SOP s xss -> SOP t xss

-- hmap :: ...

type Choice = NS I '[Int, Char, Bool]

exampleA, exampleB, exampleC :: Choice
exampleA = Z (I 3)
exampleB = S (Z (I 'x'))
exampleC = S (S (Z (I True)))


data Expr = Lit Int Char Int | Add Expr Expr
  deriving Show

type CodeExpr = '[ '[ Int, Char, Int ] , '[ Expr, Expr ] ]

{-
exampleExpr :: NS (NP I) CodeExpr
exampleExpr = S (Z (I (Lit 0) :* I (Lit 1) :* Nil))

exampleOriginalExpr :: Expr
exampleOriginalExpr = Add (Lit 0) (Lit 1)
-}

class Generic a where
  type Code a :: [[*]]

  from :: a -> Rep a
  to :: Rep a -> a

instance Generic Expr where
  type Code Expr = CodeExpr

  -- from :: Expr -> Rep Expr
  from (Lit i1 c i2) = SOP (Z    (I i1 :* I c :* I i2 :* Nil))
  from (Add e1 e2)   = SOP (S (Z (I e1 :* I e2 :* Nil)))

  -- to :: Rep Expr -> Expr
  to (SOP (Z    (I i1 :* I c :* I i2 :* Nil))) = Lit i1 c i2
  to (SOP (S (Z (I e1 :* I e2 :* Nil))))       = Add e1 e2

type Rep a = SOP I (Code a)
newtype SOP f xss = SOP (NS (NP f) xss)

-- type SOP f xss = NS (NP f) xss



class Default a where
  def :: a

instance Default Int where
  def = 0

instance Default Char where
  def = 'x'

gdef :: (Generic a, Code a ~ (xs ': xss), HPure xs, All Default xs) => a
gdef = to (SOP (Z (hcpure_NP (Proxy :: Proxy Default) (I def))))

instance Default Expr where
  def = gdef

class HPure (xs :: [k]) where
  hpure_NP :: (forall x . f x) -> NP f xs
  hcpure_NP :: All c xs => Proxy c -> (forall x . c x => f x) -> NP f xs

instance HPure '[] where
  hpure_NP _ = Nil
  hcpure_NP _ _ = Nil

instance HPure xs => HPure (x ': xs) where
  hpure_NP x = x :* hpure_NP x
  hcpure_NP p x = x :* hcpure_NP p x
