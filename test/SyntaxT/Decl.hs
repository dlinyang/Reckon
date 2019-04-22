module SyntaxT.Decl where 

import Reckon.Parser.Decl
import Reckon.Syntax
import Text.Megaparsec
import Test.Hspec
import Test.Hspec.Megaparsec

declarationTest = do
    importTest
    exportTest

importTest = describe "import package parser" $ do
    it "return a import module name" $
      parse modImp "" "import Data" `shouldParse`  ModuleDecl (ModuleImport "Data")
    
exportTest = describe "export definition paser" $ do
    it "retunr export definition"$
     parse modExp "" "export\t(sin)" `shouldParse` ModuleDecl (ModuleExport ["sin"])