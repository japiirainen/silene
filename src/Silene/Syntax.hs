-- | Syntax
--   Difference from `RawSyntax`: Here we add De Brujin Indices to the syntax.
--   These indices were not present in the `RawSyntax`.
--   If you are unfamiliar to what De Brujin Indices are here is a great intro.
--   https://www.haskellforall.com/2021/08/namespaced-de-bruijn-indices.html
module Silene.Syntax where

import Silene.RawSyntax (Name)

-- | De Brujin index.
newtype Ix = Ix Int deriving (Eq, Show, Num) via Int

-- | De Brujin level.
newtype Lvl = Lvl Int deriving (Eq, Show, Num) via Int

type Type = Term

data Term
  = Var Ix
  | Lam Name Term
  | App Term Term
  | U
  | Pi Name Type Type
  | Let Name Type Term Term
