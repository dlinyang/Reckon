module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import SyntaxT.Expr

main :: IO ()
main = hspec $ do
    varTest

--    opTest
    
--    partternTest
    