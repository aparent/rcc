module ParseJanus
  ( parseJanus
  , ModOp(..)
  , ABinOp(..)
  , BBinOp(..)
  , RBinOp(..)
  , AExpr(..)
  , BExpr(..)
  , Stmt(..)
  , Janus
  ) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token
import Control.Applicative hiding ((<|>))

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
           | VarInd String Integer
           | ABinary ABinOp AExpr AExpr
             deriving (Show)

data BExpr = BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
             deriving (Show)

--TODO: loops and such
data Stmt = ModStmt String ModOp AExpr
          | ModIndStmt String Integer ModOp AExpr
          | IfElse BExpr Stmt Stmt BExpr
          | Seq [Stmt]
            deriving (Show)

type Decl = [String]

type Janus = (Decl,Stmt)

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

brackets,parens :: Parser a -> Parser a
brackets   = Token.brackets   lexer
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
        statement' = modStatment <|>
                     ifElseStatment

modStatment :: Parser Stmt
modStatment = try (ModIndStmt <$> identifier
                              <*> brackets constant
                              <*> modOp
                              <*> aExpression)
          <|> (ModStmt <$> identifier
                       <*> modOp
                       <*> aExpression)
  where modOp = (reservedOp "+=" >> return AddM) <|>
                (reservedOp "-=" >> return SubM) <|>
                (reservedOp "^=" >> return XorM)

ifElseStatment :: Parser Stmt
ifElseStatment = IfElse <$> (reserved "if"   *> bExpression)
                        <*> (reserved "then" *> statement)
                        <*> (reserved "else" *> statement)
                        <*> (reserved "fi"   *> bExpression)

constant :: Parser Integer
constant = rd <$> many1 digit
  where rd = read :: String -> Integer

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
