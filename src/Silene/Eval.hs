-- | Evaluation
--   In this module we define the necessary batteries for evaluating
--   and quoting. This is needed for calculating the normal forms of
--   expressions.
module Silene.Eval where

import Silene.Syntax (Ix (..), Lvl (..), Term (..))
import Silene.Value (Closure (..), Env, Val (..))

-- | Closure application
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

-- | Beta-eta conversion checking.
--   This concept is explained quite well in this article.
--   https://davidchristiansen.dk/tutorials/nbe/
conv :: Lvl -> Val -> Val -> Bool
conv l t u = case (t, u) of
  (VU, VU) -> True
  (VPi _ a b, VPi _ a' b') ->
    conv l a a' && conv (l + 1) (b $$ VVar l) (b' $$ VVar l)
  (VLam _ a, VLam _ a') ->
    conv (l + 1) (a $$ VVar l) (a' $$ VVar l)
  (VLam _ a, n) ->
    conv (l + 1) (a $$ VVar l) (VApp n (VVar l))
  (n, VLam _ a) ->
    conv (l + 1) (VApp n (VVar l)) (a $$ VVar l)
  (VVar x, VVar x') -> x == x'
  (VApp a b, VApp a' b') -> conv l a a' && conv l b b'
  _ -> False
