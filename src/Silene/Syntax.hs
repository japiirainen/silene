-- | Syntax
--   Difference from `RawSyntax`: Here we add De Brujin Indices to the syntax.
--   These indices were not present in the `RawSyntax`.
--   If you are unfamiliar to what De Brujin Indices are here is a great intro.
--   https://www.haskellforall.com/2021/08/namespaced-de-bruijn-indices.html
module Silene.Syntax (Ix (..), Lvl (..), Type, Term (..), prettyTm) where

import Data.Text (Text)
import qualified Data.Text as Text
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

-- Pretty printing for syntax.
--------------------------------------------------------------------------------

fresh :: [Name] -> Name -> Name
fresh _ "_" = "_"
fresh ns x
  | x `elem` ns = fresh ns (x <> "'")
  | otherwise = x

-- | printing precedences
atomp, appp, pip, letp :: Int
atomp = 3 -- U, var
appp = 2 -- application
pip = 1 -- pi
letp = 0 -- let, lambda

-- | Wrap in parens if expression precedence is lower than
--   enclosing expression precedence
par :: Int -> Int -> ShowS -> ShowS
par p p' = showParen (p' < p)

prettyTm :: Int -> [Name] -> Term -> ShowS
prettyTm prec ns = go prec (Text.unpack <$> ns)
  where
    pt :: [Char] -> Text
    pt = Text.pack
    upt :: Text -> [Char]
    upt = Text.unpack
    piBind ns x a = showParen True ((upt x ++) . (" : " ++) . go letp ns a)
    go p ns = \case
      Var (Ix x) -> ((ns !! x) ++)
      App t u -> par p appp $ go appp ns t . (' ' :) . go atomp ns u
      Lam (fresh (pt <$> ns) -> x) t ->
        par p letp $
          ("λ " ++) . (upt x ++)
            . goLam (x : (pt <$> ns)) t
        where
          goLam ns (Lam (fresh ns -> x) t) =
            (' ' :) . (upt x ++) . goLam (x : ns) t
          goLam ns t =
            (". " ++) . go letp (upt <$> ns) t
      U -> ("U" ++)
      Pi "_" a b -> par p pip $ go appp ns a . (" → " ++) . go pip ("_" : ns) b
      Pi (fresh (pt <$> ns) -> x) a b ->
        par p pip $
          piBind ns x a
            . goPi (upt x : ns) b
        where
          goPi :: [String] -> Type -> [Char] -> [Char]
          goPi ns (Pi "_" a b) =
            (" → " ++) . go appp ns a . (" → " ++) . go pip ("_" : ns) b
          goPi ns (Pi (fresh (pt <$> ns) -> x) a b) =
            piBind ns x a . goPi (upt x : ns) b
          goPi ns b =
            (" → " ++) . go pip ns b
      Let (fresh (pt <$> ns) -> x) a t u ->
        par p letp $
          ("let " ++) . (upt x ++) . (" : " ++) . go letp ns a
            . ("\n    = " ++)
            . go letp ns t
            . ("\nin\n" ++)
            . go letp (upt x : ns) u

instance Show Term where showsPrec p = prettyTm p []
