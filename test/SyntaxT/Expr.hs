module SyntaxT.Expr where 

import Reckon.Parser.Expr
import Reckon.Syntax
import Text.Megaparsec
import Test.Hspec
import Test.Hspec.Megaparsec

exprTest = do
    varTest
    opTest
    ifTest
    caseExprTest

varTest = describe "Variable parser" $ do
    it "return a variable AST" $
        parse variable "" "x" `shouldParse`  Var "x"

opTest = describe "operator parser" $do
    it "return a op AST" $
        parse operator "" ">=" `shouldParse` Var ">="

ifTest = describe "condition parser" $ do
    it "return a condition AST" $
        parse ifExpr "" "if a then b else c" `shouldParse` If (Var "a") (Var "b") (Var "c")
    it "return a condition AST" $
        parse ifExpr "" "if a\n\tthen b\n\telse c" `shouldParse` If (Var "a") (Var "b") (Var "c")

caseExprTest = describe "case expr parser" $do
    it "return a case AST"$
        parse caseExpr "" "case x of\n x >= 1 => 1" `shouldParse` 
        Case (Var "x") [(Ap (Ap (Var ">=") (Var "x")) (Literal (RInteger 1)),Literal (RInteger 1))]
    it "return a case AST"$
        parse caseExpr "" "case x of\n  x >= 1 => 1\n  x < 1 => 0" `shouldParse`
        Case (Var "x") [(Ap (Ap (Var ">=") (Var "x")) (Literal (RInteger 1)),Literal (RInteger 1)),
                            (Ap (Ap (Var "<")  (Var "x")) (Literal (RInteger 1)),Literal (RInteger 0))]