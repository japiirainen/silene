module Silene (main) where

import Control.Monad.Except (runExcept)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Silene.Elab as Elab
import qualified Silene.Eval as Eval
import qualified Silene.Parser as Parser
import System.Environment (getArgs)
import Text.Megaparsec (SourcePos (..), initialPos)
import Text.Megaparsec.Pos (unPos)
import Text.Printf (printf)

-- TODO: proper CLI with optparse-applicative

help :: String
help =
  unlines
    [ "usage: silene [--help|parse]",
      "  --help           : display this message",
      "  parse <fileName> : read & parse expression, print parse result",
      "  nf    <filename> : typecheck expression from a file, print normal form",
      "  type  <filename> : typecheck expression from a file, print type"
    ]

displayError :: String -> Elab.ElabError -> IO ()
displayError
  file
  (Elab.ElabError (msg, SourcePos path (unPos -> linum) (unPos -> colnum))) =
    do
      let lnum = show linum
          lpad = map (const ' ') lnum
      printf "%s:%d:%d:\n" path linum colnum
      printf "%s |\n" lpad
      printf "%s | %s\n" lnum (lines file !! (linum - 1))
      printf "%s | %s\n" lpad (replicate (colnum - 1) ' ' ++ "^")
      printf "%s\n" msg

main :: IO ()
main = do
  getArgs >>= \case
    ["--help"] -> putStrLn help
    ["nf", fp] -> do
      content <- TIO.readFile fp
      case Parser.parseText content of
        Left err -> putStrLn err
        Right t ->
          case runExcept $
            Elab.infer (Elab.emptyCtxt (initialPos (Text.unpack content))) t of
            Left err -> displayError (Text.unpack content) err
            Right (t, a) -> do
              print $ Eval.nf [] t
              putStr "  :"
              print $ Eval.quote 0 a
    ["type", fp] -> do
      content <- TIO.readFile fp
      case Parser.parseText content of
        Left err -> putStrLn err
        Right t ->
          case runExcept $
            Elab.infer (Elab.emptyCtxt (initialPos (Text.unpack content))) t of
            Left err -> displayError (Text.unpack content) err
            Right (t, a) -> print $ Eval.quote 0 a
    ["parse", fp] -> do
      content <- TIO.readFile fp
      either putStrLn print (Parser.parseText content)
    _ -> putStrLn help
