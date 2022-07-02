-- | Evaluation
--   In this module we define the necessary batteries for evaluating
--   and quoting. This is needed for calculating the normal forms of
--   expressions.
module Silene.Eval where

import Silene.Syntax (Ix (..), Lvl (..), Term (..))
import Silene.Value (Closure (..), Env, Val (..))

($$) :: Closure -> Val -> Val
(Closure env t) $$ u = eval (u : env) t

eval :: Env -> Term -> Val
eval env = \case
  Var (Ix x) -> env !! x
  App t1 t2 -> case (eval env t1, eval env t2) of
    (VLam _ clos, u) -> clos $$ u
    (t, u) -> VApp t u
  Lam name t -> VLam name (Closure env t)
  Pi name a b -> VPi name (eval env a) (Closure env b)
  Let _ _ t u -> eval (eval env t : env) u
  U -> VU

lvl2Ix :: Lvl -> Lvl -> Ix
lvl2Ix (Lvl l) (Lvl l') = Ix (l - l' - 1)

quote :: Lvl -> Val -> Term
quote l = \case
  VVar x -> Var (lvl2Ix l x)
  VApp t u -> App (quote l t) (quote l u)
  VLam name t -> Lam name (quote (l + 1) (t $$ VVar l))
  VPi name a b -> Pi name (quote l a) (quote (l + 1) (b $$ VVar l))
  VU -> U

nf :: Env -> Term -> Term
nf env t = quote (Lvl (length env)) (eval env t)
