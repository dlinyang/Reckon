module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import SyntaxT.Expr
import SyntaxT.Decl

main :: IO ()
main = hspec $ do
    declarationTest

    exprresionTest