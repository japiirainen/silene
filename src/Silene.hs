module Silene (main) where

import qualified Data.Text.IO as TIO
import qualified Silene.Parser as Parser
import System.Environment (getArgs)

-- TODO: proper CLI with optparse-applicative

help :: String
help =
  unlines
    [ "usage: silene [--help|parse]",
      "  --help           : display this message",
      "  parse <fileName> : read & parse expression, print parse result"
    ]

main :: IO ()
main = do
  getArgs >>= \case
    ["--help"] -> putStrLn help
    ["parse", fileName] -> do
      content <- TIO.readFile fileName
      either putStrLn (putStrLn . show) (Parser.parseText content)
    _ -> putStrLn help
