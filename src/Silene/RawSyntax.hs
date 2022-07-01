-- | Here we define the `raw` syntax for our language.
--   This corresponds to the output of our `Parser`.
module Silene.RawSyntax (Name, Raw (..)) where

import Data.Text (Text)
import Text.Megaparsec (SourcePos)

-- | Name of a variable.
type Name = Text

-- | Raw representation of the syntax after parsing.
data Raw
  = RVar Name -- x
  | RLam Name Raw -- \x. t -- let f: A -> B = \x -> ...
  | RApp Raw Raw -- t u
  | RU -- U
  | RPi Name Raw Raw -- (x: A) -> B
  | RLet Name Raw Raw Raw -- let x: A = t in u
  | RSrcPos SourcePos Raw -- source position for error reporting
  deriving (Show)
