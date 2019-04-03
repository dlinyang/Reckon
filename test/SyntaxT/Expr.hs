module SyntaxT.Expr where 

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

varTest = it "return variable asr" $do
    (testParse variable "x") `shouldBe` (Var "x")

partternTest = it "return layout case expresion ast" $do
    (testParse parttern "case x of \n\t x => x ") `shouldBe` (Parttern (Var "x") [(Var"x",Var"x")])