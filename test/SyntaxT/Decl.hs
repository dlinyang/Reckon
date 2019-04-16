module SyntaxT.Decl where 

import Reckon.Parser.Decl
import Reckon.Syntax
import Text.Megaparsec
import Test.Hspec
import Test.Hspec.Megaparsec

declarationTest = do
    importTest

importTest = describe "Variable parser" $ do
    it "return a import module name" $
      parse modImp "" "import Data" `shouldParse`  ModuleDecl (MImp "Data")