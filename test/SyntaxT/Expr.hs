module SyntaxT.Expr where 

import Reckon.Parser.Expr
import Reckon.Syntax
import Text.Megaparsec
import Test.Hspec
import Test.Hspec.Megaparsec

varTest = describe "Variable parser" $ do
    it "return a variable ast" $
        parse variable "" "x" `shouldParse`  Var "x"

opTest = describe "operator parser" $do
    it "return a op ast" $
        parse operator ">=" `shouldParse` Var ">="

partternTest = parseTest parttern "case x of\n\tx >= 1 => 1\n\tx >= 2 => 2"