module SyntaxT.Expr where 

import Reckon.Parser.Expr
import Reckon.Syntax
import Text.Megaparsec
import Test.Hspec
import Test.Hspec.Megaparsec

varTest = describe "Variable parser" $ do
    it "return a variable AST" $
        parse variable "" "x" `shouldParse`  Var "x"

opTest = describe "operator parser" $do
    it "return a op AST" $
        parse operator "" ">=" `shouldParse` Var ">="

partternTest = describe "parttern case" $do
    it "return a case AST"$
        parse parttern "" "case x of\n\tx >= 1 => 1" `shouldParse` 
        Parttern (Var "x") [(Ap (Ap (Var ">=") (Var "x")) (Literal (RInteger 1)),Literal (RInteger 1))]
    it "return a case AST"$
        parse parttern "" "case x of\n\tx >= 1 => 1\n\tx < 1 => 0" `shouldParse`
        Parttern (Var "x") [(Ap (Ap (Var ">=") (Var "x")) (Literal (RInteger 1)),Literal (RInteger 1)),
                            (Ap (Ap (Var "<")  (Var "x")) (Literal (RInteger 1)),Literal (RInteger 0))]