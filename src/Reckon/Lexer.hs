module Reckon.Lexer where 

import Control.Monad (void)
import Control.Monad.Combinators
import Control.Monad.Combinators.Expr
import Data.Void
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

scn::Parser ()
scn = L.space space1 lineComment blockComment

lineComment = L.skipLineComment "--"
blockComment = L.skipBlockComment "{-" "-}"

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc 

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

accent :: Parser a -> Parser a
accent = between (symbol "`") (symbol "`")

charLit :: Parser Char
charLit = between (symbol "\'") (symbol "\'") L.charLiteral

stringLit :: Parser String
stringLit = char '\"' *> manyTill L.charLiteral (char '\"')

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Float
float = lexeme L.float

reservedWords' :: [String]
reservedWords' = ["let","if","then","else","case","of","where","do",
                  "abstract","concrete","Cat",
                  "import","export","module","hiding","syntax","foreign"]

reservedWords :: String -> Parser ()
reservedWords w = (lexeme.try) (string w *> notFollowedBy alphaNumChar)

opChars :: String
opChars = ":!#$%*+./<=>/@\\^|-~"

opLetter = oneOf opChars

reservedOp' :: [String]
reservedOp' = [":",".","\\","->","=>","=",":=","|",",", --
               "+","-","*","/","%"]                     --

cmmnt :: [String]
cmmnt = ["--","{-","-}"]

reservedOp :: String -> Parser ()
reservedOp op = (lexeme . try) (string  op *> notFollowedBy opLetter)

symbolicOp :: Parser String
symbolicOp = (lexeme . try ) (p >>= check )
    where 
        p       = (:) <$> opLetter <*> many  opLetter
        check x = if x `elem` (reservedOp' ++ cmmnt)
                    then fail $ show x ++ "is not a valid operator" 
                    else return x

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedWords'
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x