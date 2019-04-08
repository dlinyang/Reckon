module SyntaxT.Expr where 

import Reckon.Parser.Expr
import Reckon.Syntax
import Text.Megaparsec

varTest = parseTest variable "x"

opTest = parseTest operator ">="

partternTest = parseTest parttern "case x of\n\tx >= 1 => 1\n\tx >= 2 => 2"