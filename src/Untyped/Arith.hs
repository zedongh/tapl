module Untyped.Arith (Term(..), arith, eval, parseMaybe) where

import Control.Monad.Combinators.Expr
import Control.Monad (replicateM)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Term =
      TTrue              -- true                ; terms
    | TFalse             -- false               ; constant false
    | TIf Term Term Term  -- if t then t else t  ; conditional
    | TZero              -- 0                   ; constant zero
    | TSucc Term         -- succ                ; successor
    | TPred Term         -- pred                ; predecessor
    | TIsZero Term       -- iszero              ; test
    deriving (Show, Eq)

-- parser 
type Parser = Parsec Void String

lexeme
    :: Parser a -- How to consume white space after lexeme
    -> Parser a -- How to parse actual lexeme
lexeme = L.lexeme space

reserve :: String -> Parser ()
reserve str = (lexeme . try) (string' str *> notFollowedBy alphaNumChar)

symbol :: String -> Parser String
symbol = L.symbol space

paren = between (symbol "(") (symbol ")")

_arith :: Parser Term
_arith = TTrue <$ reserve "true"
     <|> TFalse <$ reserve "false"
     <|> TIf <$> (reserve "if" >> _arith) <*> (reserve "then" >> _arith) <*> (reserve "else" >> _arith)
     <|> TZero <$ symbol "0"
     <|> TSucc <$> (reserve "succ" >> paren _arith)
     <|> TPred <$> (reserve "pred" >> paren _arith)
     <|> TIsZero <$> (reserve "iszero" >> paren _arith)

arith = between space eof _arith

-- evaluation
isNumericVal :: Term -> Bool
isNumericVal expr =
    case expr of
        TZero -> True
        TSucc t -> isNumericVal t
        TPred TZero -> False                -- Zero haven't pred, peano axiom
        TPred t -> isNumericVal t
        _ -> False

isVal :: Term -> Bool
isVal expr =
    case expr of
        TTrue -> True
        TFalse -> True
        t | isNumericVal t -> True
        _ -> False

eval :: Term -> Term
eval expr =
    case expr of
        TIf TTrue t2 t3 -> eval t2
        TIf TFalse t2 t3 -> eval t3
        TIf t1 t2 t3 ->  eval $ TIf (eval t1) t2 t3
        TSucc t -> eval . TSucc $ eval t
        TPred (TSucc t) | isNumericVal t -> eval t
        TPred t -> eval . TPred $ eval t
        TIsZero TZero -> TTrue
        TIsZero (TSucc t) | isNumericVal t -> TFalse
        TIsZero t -> eval . TIsZero $ eval t
        _ -> expr