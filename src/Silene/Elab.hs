-- | Elaboration
module Silene.Elab (check, infer, emptyCtxt, ElabError (..)) where

import qualified Control.Monad as Monad
import Control.Monad.Error.Class (MonadError (..))
import qualified Data.Text as Text
import Silene.Eval (conv, eval, quote, ($$))
import Silene.RawSyntax (Name, Raw (..))
import Silene.Syntax (Lvl (..), Term (..), prettyTm)
import Silene.Value (Env, VType, Val (..))
import Text.Megaparsec (SourcePos)
import Text.Printf (printf)

-- | Variable Name -> Type mapping for every variable in the current scope
type Types = [(Name, VType)]

-- | Elaboration context
data Ctx = Ctx
  { env :: Env,
    types :: Types,
    lvl :: Lvl,
    pos :: SourcePos
  }

-- | Empty elaboration context
emptyCtxt :: SourcePos -> Ctx
emptyCtxt = Ctx [] [] 0

-- | Extend the context with a bound variable
bind :: Name -> VType -> Ctx -> Ctx
bind x ~a (Ctx e ts l p) =
  Ctx (VVar l : e) ((x, a) : ts) (l + 1) p

-- | Extend the context with a definition
define :: Name -> Val -> VType -> Ctx -> Ctx
define x ~t ~a (Ctx e ts l p) =
  Ctx (t : e) ((x, a) : ts) (l + 1) p

-- | Elaboration monad. We annotate the error with the current sounce position.
newtype ElabError = ElabError (String, SourcePos)
  deriving newtype (Show)

report :: MonadError ElabError m => Ctx -> String -> m a
report ctx msg = throwError (ElabError (msg, pos ctx))

showVal :: Ctx -> Val -> String
showVal ctx v = prettyTm 0 (map fst (types ctx)) (quote (lvl ctx) v) []

-- bidirectional algorithm:
--   use check when the type is already known
--   use infer if the type is unknown

check :: MonadError ElabError m => Ctx -> Raw -> VType -> m Term
check ctx t a = case (t, a) of
  (RSrcPos pos t, a) -> check (ctx {pos = pos}) t a
  -- checking lambda with pi type (canonical checking case)
  -- (\x. t) : ((x : A) -> B)
  (RLam x t, VPi _ a b) ->
    Lam x <$> check (bind x a ctx) t (b $$ VVar (lvl ctx))
  -- fall-through checking
  -- let x : a = t in u
  (RLet x a t u, a') -> do
    a <- check ctx a VU
    let ~va = eval (env ctx) a
    t <- check ctx t va
    let ~vt = eval (env ctx) t
    u <- check (define x vt va ctx) u a'
    pure (Let x a t u)

  -- only lambda and let is checkable
  -- if the term is not checkable, we switch to inferring
  -- this is the essence of bidirectional algorithm
  _ -> do
    (t, tty) <- infer ctx t
    Monad.unless (conv (lvl ctx) tty a) $
      report
        ctx
        ( printf
            "Type mismatch:\n\nexpected type:\n\n  %s\n\ninferred type:\n\n %s\n"
            (showVal ctx a)
            (showVal ctx tty)
        )
    pure t

infer :: MonadError ElabError m => Ctx -> Raw -> m (Term, VType)
infer ctx = \case
  RSrcPos pos t -> infer (ctx {pos = pos}) t
  RVar x -> do
    let go _ [] = report ctx ("Variable out of scope: " <> Text.unpack x)
        go i ((x', a) : tys)
          | x == x' = pure (Var i, a)
          | otherwise = go (i + 1) tys
    go 0 (types ctx)
  RU -> pure (U, VU)
  RApp t u -> do
    (t, tty) <- infer ctx t
    case tty of
      VPi _ a b -> do
        u <- check ctx u a
        pure (App t u, b $$ eval (env ctx) u) -- t u : B[x |-> u]
      tty ->
        report ctx $
          "Expected a function type, instead inferred:\n\n  "
            <> showVal ctx tty
  RLam {} -> report ctx "Can't infer type for lambda expression"
  RPi x a b -> do
    a <- check ctx a VU
    b <- check (bind x (eval (env ctx) a) ctx) b VU
    pure (Pi x a b, VU)
  RLet x a t u -> do
    a <- check ctx a VU
    let ~va = eval (env ctx) a
    t <- check ctx t va
    let ~vt = eval (env ctx) t
    (u, uty) <- infer (define x vt va ctx) u
    pure (Let x a t u, uty)
