module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Control.Applicative ((<$>), (<*>))

import Lexer
import Syntax

binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc

table = [ [ binary "*" Times Ex.AssocLeft
          , binary "/" Divide Ex.AssocLeft]
        , [ binary "+" Plus Ex.AssocLeft
          , binary "-" Minus Ex.AssocLeft]]

int :: Parser Expr
int = (Float . fromInteger) <$> integer

floating :: Parser Expr
floating = Float <$> float

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Expr
function = reserved "def" >>
           Function <$> identifier <*> (parens $ many variable) <*> expr

extern :: Parser Expr
extern = reserved "extern" >> Extern <$> identifier <*> (parens $ many variable)

call :: Parser Expr
call = Call <$> identifier <*> (parens $ commaSep expr)

factor :: Parser Expr
factor =  try floating
      <|> try int
      <|> try extern
      <|> try function
      <|> try call
      <|> variable
      <|> parens expr

defn :: Parser Expr
defn =  try extern
    <|> try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
             def <- defn
             reservedOp ";"
             return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s
