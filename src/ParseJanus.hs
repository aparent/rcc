module ParseJanus
  ( parseJanus
  , arbitraryJanus
  , ModOp(..)
  , ABinOp(..)
  , BBinOp(..)
  , RBinOp(..)
  , AExpr(..)
  , BExpr(..)
  , Stmt(..)
  , Janus
  ) where

import Prelude
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token
import Control.Applicative hiding ((<|>))

import Data.List
import Test.QuickCheck

-- | Reversible modification operators
data ModOp = AddM
           | SubM
           | XorM
             deriving (Show)

-- | Arithmetic binary operators
data ABinOp = Add
            | Sub
            | Xor
            | Mult
            | Div
            | Mod
            | And
            | Or
            deriving (Show)

-- | Binary operators
data BBinOp = BAnd
            | BOr
              deriving (Show)

-- | Relational operators
data RBinOp = RGT
            | RLT
            | RGTE
            | RLTE
            | RNE
            | RE
              deriving (Show)

data AExpr = ConstInt Integer
           | Var String
           | ABinary ABinOp AExpr AExpr
             deriving (Show)

data BExpr = BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
             deriving (Show)

data Stmt = ModStmt String ModOp AExpr
          --Supporting loops with constant bounds only
          | Loop String Integer Stmt Integer
          | IfElse BExpr Stmt Stmt BExpr
          | Seq [Stmt]
            deriving (Show)

type Decl = [String]

type Janus = (Decl,Stmt)

--Here we define how to make an arbitrary program for testing purposes

instance Arbitrary ModOp where
  arbitrary = elements [AddM, SubM, XorM]

instance Arbitrary ABinOp where
  arbitrary = elements [Add, Sub, Xor, Mult, Div, Mod, And, Or]

instance Arbitrary BBinOp where
  arbitrary = elements [BAnd, BOr]

instance Arbitrary RBinOp where
  arbitrary = elements [RGT, RLT, RGTE, RLTE, RNE, RE]

arbitraryJanus :: Gen Janus
arbitraryJanus =
  do
  vars <- nub <$> arbitrary `suchThat` ( (2<) . length )
  stmt <- mkStmt vars
  return (vars , stmt)
  where mkStmt vs = oneof [mkModStmt]
          where mkModStmt = do var <- elements vs
                               ModStmt var <$> arbitrary <*> mkAExpr (vs \\ [var])
        mkAExpr vs = oneof
                       [ ConstInt <$> choose (0,32)
                       , Var <$> elements vs
                       , ABinary <$> arbitrary <*> mkAExpr vs <*> mkAExpr vs
                       ]


lexer :: Token.TokenParser st
lexer = Token.makeTokenParser languageDef
  where languageDef = emptyDef { Token.commentStart = "/*"
                               , Token.commentEnd  = "*/"
                               , Token.commentLine = "//"
                               , Token.identStart  = letter
                               , Token.identLetter = alphaNum
                               , Token.reservedNames = [ "if"
                                                       , "fi"
                                                       , "then"
                                                       , "else"
                                                       , "while"
                                                       , "from"
                                                       , "loop"
                                                       , "do"
                                                       , "until"
                                                       , "call"
                                                       , "uncall"
                                                       , "skip"
                                                        ]
                               , Token.reservedOpNames = ["-=", "+=", "^="
                                                        , "+", "-", "*", "^", "%", "/", "&", "|"
                                                        , "=" , "<=", ">=", "!=", "<", ">"
                                                        , "||", "&&"
                                                        ]
                               }

identifier,semi :: Parser String
identifier = Token.identifier lexer
semi       = Token.semi       lexer

reserved,reservedOp :: String -> Parser ()
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens     = Token.parens     lexer

integer :: Parser Integer
integer    = Token.integer    lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

type Parser = Parsec String ()

parseJanus :: String -> String -> Either ParseError Janus
parseJanus = parse janus

janus :: Parser Janus
janus = do whiteSpace
           (,) <$> declarations
               <*> statement

declarations :: Parser [String]
declarations = many1 identifier <* semi

statement :: Parser Stmt
statement = parens statement <|>
            sequenceOfStmt
  where sequenceOfStmt =
          do list <- endBy1 statement' semi
             return $ if length list == 1 then head list else Seq list
        statement' = modStatment
                 <|> ifElseStatment
                 <|> loopStatment

modStatment :: Parser Stmt
modStatment = ModStmt <$> identifier
                      <*> modOp
                      <*> aExpression
  where modOp = (reservedOp "+=" >> return AddM) <|>
                (reservedOp "-=" >> return SubM) <|>
                (reservedOp "^=" >> return XorM)

ifElseStatment :: Parser Stmt
ifElseStatment = IfElse <$> (reserved "if"   *> bExpression)
                        <*> (reserved "then" *> statement)
                        <*> (reserved "else" *> statement)
                        <*> (reserved "fi"   *> bExpression)
loopStatment :: Parser Stmt
loopStatment = Loop <$> (reserved "loop" *> identifier)
                    <*> (reservedOp "=" *> integer)
                    <*> statement
                    <*> (reserved "until" *> integer)

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm
  where aTerm = parens aExpression <|>
                Var <$> identifier <|>
                ConstInt <$> integer
        aOperators = [
                       [
                         Infix (reservedOp "*" >> return (ABinary Mult)) AssocLeft,
                         Infix (reservedOp "/" >> return (ABinary Div)) AssocLeft,
                         Infix (reservedOp "%" >> return (ABinary Mod)) AssocLeft
                       ],
                       [
                         Infix (reservedOp "+" >> return (ABinary Add)) AssocLeft,
                         Infix (reservedOp "-" >> return (ABinary Sub)) AssocLeft
                       ],
                       [
                         Infix (reservedOp "&" >> return (ABinary And)) AssocLeft,
                         Infix (reservedOp "^" >> return (ABinary Xor)) AssocLeft,
                         Infix (reservedOp "|" >> return (ABinary Or)) AssocLeft
                       ]
                     ]



bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm
  where bTerm = parens bExpression <|>
                rExpression
        rExpression =
          do a1 <- aExpression
             op <- relation
             a2 <- aExpression
             return $ RBinary op a1 a2
          where relation = (reservedOp ">"  >> return RGT ) <|>
                           (reservedOp "<"  >> return RLT ) <|>
                           (reservedOp "<=" >> return RLTE) <|>
                           (reservedOp ">=" >> return RGTE) <|>
                           (reservedOp "!=" >> return RNE ) <|>
                           (reservedOp "="  >> return RE  )
        bOperators = [
                      [
                        Infix (reservedOp "&&" >> return (BBinary BAnd)) AssocLeft,
                        Infix (reservedOp "||" >> return (BBinary BOr)) AssocLeft
                      ]
                     ]
