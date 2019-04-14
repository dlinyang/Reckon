module Main where

import Control.Monad.Trans
import System.Console.Haskeline
import Text.Megaparsec

import Reckon.Parser

process :: String -> IO ()
process line = do
  let res = parseStatement line
  case res of
    Left err -> putStr (errorBundlePretty err)
    Right ex -> print ex

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "reckon>"
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop