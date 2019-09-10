module Parser where

import Lexer
import Syntax

import Data.Functor.Identity (Identity)

import           Text.Parsec
import qualified Text.Parsec.Expr  as Ex
import           Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

-- | function that actually takes a string and parses it to an Expr using the parsers below
parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

-- | function that actually takes a string and parses it to a program using the parsers below
parseProgram :: String -> Either ParseError [Expr]
parseProgram s = parse (contents program) "<stdin>" s

-- | turn our parser into something that parses a whole file's contents
contents :: Parser a -> Parser a
contents p = Tok.whiteSpace lexer *> p <* eof

-- | parses a program, which is a sequence of 'lines'
program :: Parser [Expr]
program = many (line <* reservedOp ";")

-- | parses a toplevel external declaration, function declaration, or expression
line :: Parser Expr
line = try extern
    <|> try function
    <|> expr

-- | parses some arbitrary expression into the AST
expr :: Parser Expr
expr = Ex.buildExpressionParser table term

-- | table of binary operators
table :: Ex.OperatorTable String () Identity Expr
table = [[ binary "*" Times Ex.AssocLeft, binary "/" Divide Ex.AssocLeft]
        ,[ binary "+" Plus Ex.AssocLeft, binary "-" Minus Ex.AssocLeft]
        ]

-- | convenience function for declaring binary operator
binary :: String -> Op -> Ex.Assoc -> Ex.Operator String () Identity Expr
binary name op assoc = Ex.Infix (reservedOp name >> return (BinOp op)) assoc

-- | parse a term in an Expr
term :: Parser Expr
term = try floating
    <|> try int
    <|> try extern
    <|> try function
    <|> try call
    <|> variable
    <|> parens expr

floating :: Parser Expr
floating = Float <$> float 

int :: Parser Expr
int = (Float . fromInteger) <$> integer

extern :: Parser Expr
extern = Extern 
      <$> (reserved "extern" *> identifier) 
      <*> parens (many identifier)

variable :: Parser Expr
variable = Var <$> identifier 

function :: Parser Expr
function = Function 
        <$> (reserved "def" *> identifier) 
        <*> parens (many identifier) 
        <*> expr

call :: Parser Expr
call = Call <$> identifier <*> parens (commaSep expr)
