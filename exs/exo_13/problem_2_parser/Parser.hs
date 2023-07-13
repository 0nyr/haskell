module Parser where

import Control.Monad
import Text.Parsec
import Text.Parsec.Language ( javaStyle )
import qualified Text.Parsec.Token as P
import Text.Parsec.Token ( TokenParser, makeTokenParser )
import System.IO

import Expr

type Parser a = Parsec String () a

--
-- Parse a string.
-- For example,
--   parseExpr "Y (\\f -> \\n -> f (n+1))"
-- should yield
--  Y (Lambda "f" (Lambda "n" (App (Var "f") (BinOp Add (Var "n") (Const 1)))))
--
parseExpr :: String -> Expr
parseExpr str =
    case parse startexpr "" str of
      Right p  -> p
      Left err -> error (show err)

-- We use Parsec's build-in functionality to deal with
-- whitespace, etc. and the ambiguities that arise from
-- the fact that keywords look like identifiers (e.g.,
-- "if" is a keyword but "ifx" is an identifier).
lexer :: TokenParser ()
lexer  = makeTokenParser
         (javaStyle
          { P.reservedOpNames = ["+", "-", "*", "\\", "->"]
          , P.reservedNames = ["if", "then", "else", "Y"]
          })

-- parse a natural number
natural :: Parser Integer
natural = P.natural lexer

-- parse an expression in parentheses, e.g.
--  parens natual
-- would parse a natural number in parentheses.
parens :: Parser a -> Parser a
parens = P.parens lexer

-- parse an identifier
identifier :: Parser String
identifier = P.identifier lexer

-- parse a reserved keyword (one of the "reservedNames" above in "lexer"),
-- e.g., use   reserved "if"  to parse the keyword "if"
reserved :: String -> Parser ()
reserved = P.reserved lexer

-- parse an operator (one of the "reservedOpNames" above in "lexer"),
-- e.g., use  reservedOp "+"  to parse the operator for addition
reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer


-- Entry function for parsing. Make sure that
-- we parse until the of the input.
startexpr :: Parser Expr
startexpr = do
  e <- expr
  eof
  return e

-- copy-paste the following test string into the terminal:
-- (Y (\fib -> \n -> if n then if n-1 then fib (n-1) + fib (n-2) else 1 else 0)) 12
--
-- compile:
-- ```shell
--  ❮onyr ★ kenzael❯ ❮exo_13❯❯ ghc -o Main Main.hs -package parsec 
-- Loaded package environment from /home/onyr/.ghc/x86_64-linux-9.2.7/environments/default
-- [2 of 3] Compiling Parser           ( Parser.hs, Parser.o )
-- Linking Main ...
-- ```
--
-- run:
-- ```shell
-- ❮onyr ★ kenzael❯ ❮exo_13❯❯ ./Main 
-- (Y (\fib -> \n -> if n then if n-1 then fib (n-1) + fib (n-2) else 1 else 0)) 12
-- 144
-- ```
--
-- Grammar:
--
-- <expr> ::=  '\' <identifier> '->' <expr>             -- lambda abstraction
--          |  'if' <expr> 'then' <expr> 'else' <expr>
--          |  <texpr>
--
-- <texpr> ::= <aexpr> <op> <aexpr>                     -- binary operation
--           | <aexpr>
--
-- <aexpr> ::= 'Y' <pexpr>                              -- fixed-point operator
--           | <pexpr> <pexpr>                          -- application
--           | <pexpr>
--
-- <pexpr> ::= '(' <expr> ')'
--           | <identifier>
--           | <natural>
--
-- <op> ::= '+' | '-' | '*'
--

--
-- Notes about the implementation:
-- * <|> and <$> are operators in Haskell that come from the libraries 
-- Control.Applicative and Control.Monad.
--
-- * <|> is an operator from the Control.Applicative module. 
-- It's known as the alternative operator. If the parser before <|> fails, 
-- it will try to use the parser after <|>. So try parser1 <|> parser2 
-- means "try parser1, and if that fails, use parser2".
--
-- * <$> is an operator from the Control.Applicative module, and it's a synonym 
-- for the fmap function. It's used to apply a function to a value in a context. 
-- In the context of the Parsec library, it applies a function to the result of 
-- a parser. For example, Var <$> identifier means "parse an identifier, and 
-- then apply the Var function to the result".
--
-- To summarize, <$> is used to apply a function to a value in a functor context 
-- (like Maybe, a parser in Parsec, a list, etc.), and <|> is used to provide 
-- alternative computations in an applicative context, and in case of parsers 
-- it's used for offering alternative parses.
--

expr :: Parser Expr
expr = try lambdaExpr <|> ifThenElseExpr <|> termExpr

lambdaExpr :: Parser Expr
lambdaExpr = do
    reservedOp "\\"
    var <- identifier
    reservedOp "->"
    body <- expr
    return (Lambda var body)

ifThenElseExpr :: Parser Expr
ifThenElseExpr = do
    reserved "if"
    cond <- expr
    reserved "then"
    trueExpr <- expr
    reserved "else"
    falseExpr <- expr
    return (IfThenElse cond trueExpr falseExpr)

termExpr :: Parser Expr
termExpr = do
    left <- appExpr
    (try (binaryOp left) <|> return left)

binaryOp :: Expr -> Parser Expr
binaryOp left = do
    op <- operator
    right <- appExpr
    return (BinOp op left right)

appExpr :: Parser Expr
appExpr = do
    left <- atomExpr
    (try (appOp left) <|> return left)

appOp :: Expr -> Parser Expr
appOp left = do
    right <- atomExpr
    return (App left right)

atomExpr :: Parser Expr
atomExpr = try yExpr <|> primaryExpr

yExpr :: Parser Expr
yExpr = do
    reserved "Y"
    body <- primaryExpr
    return (Y body)

primaryExpr :: Parser Expr
primaryExpr = parens expr <|> variable <|> constant

variable :: Parser Expr
variable = Var <$> identifier

constant :: Parser Expr
constant = Const <$> natural

operator :: Parser Op
operator = choice
    [ reservedOp "+" *> return Add
    , reservedOp "-" *> return Sub
    , reservedOp "*" *> return Mul
    ]
