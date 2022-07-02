-- | TODO document this module
module Silene.Value where

import Silene.RawSyntax (Name)
import Silene.Syntax (Lvl, Term)

type Env = [Val]

data Closure = Closure Env Term

type VType = Val

data Val
  = VVar Lvl
  | VApp Val ~Val
  | VLam Name {-# UNPACK #-} Closure
  | VPi Name ~VType {-# UNPACK #-} Closure
  | VU
