-- INTRODUCTION TO TYPE-LEVEL AND
-- GENERIC PROGRAMMING IN HASKELL
-- WITH GENERICS-SOP
--
-- Andres Löh, Well-Typed
-- CUFP 2016
-- 2016-09-22

-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE UndecidableSuperClasses #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE DefaultSignatures #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module CUFP1 where

import Generics.SOP
import Generics.SOP.TH

-- Example datatypes.

-- data Maybe a = Nothing | Just a
-- data Either a b = Left a | Right b

data Group = Group Char Bool Int

data Expr =
    NumL Int
  | BoolL Bool
  | Add Expr Expr
  | If Expr Expr Expr

-- Example 1. Generic (non-recursive) "coerce".

-- Example 2. Generic "size".
--
-- Count the number of constructors.

-- Example 3. Generic "default value".

-- Example 4. Generic monoid.

-- Example 5. Generic enumeration.

-- ========================================================================

-- Example 6. Constructor name(s).

-- Example 7. A simple show function.

-- Example 8. A simple generic editor.

{-

class SimpleShow a where
  simpleShow :: a -> String

instance SimpleShow Int where
  simpleShow = show

instance SimpleShow String where
  simpleShow = show

conName :: forall a . (Generic a, HasDatatypeInfo a) => a -> String
conName x =
  go (datatypeInfo (Proxy :: Proxy a) ^. constructorInfo) (unSOP (from x))
  where
    go p s = hcollapse (hzipWith (\ ci _ -> K (ci ^. constructorName)) p s)

gsimpleShow ::
  forall a . (Generic a, HasDatatypeInfo a, All2 SimpleShow (Code a)) =>
  a -> String
gsimpleShow a =
  ( parens
  . intercalate " "
  . prefixcon
  . hcollapse
  . hcmap (Proxy :: Proxy SimpleShow) (\ (I x) -> K (simpleShow x))
  . from
  ) a
  where
    prefixcon = (conName a :)
    parens x = "(" ++ x ++ ")"

-}
