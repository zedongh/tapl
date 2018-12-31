module Untyped.Lambda (Term(..), term, parseTerm) where 
    

import Control.Monad.Combinators.Expr
import Control.Monad (replicateM)
import Control.Monad.State
import Data.Void
import Data.Char (toLower)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (abs)

-- parser 
type Parser = ParsecT Void String (State Context)

lexeme
    :: Parser a -- How to consume white space after lexeme
    -> Parser a -- How to parse actual lexeme
lexeme = L.lexeme space

reserve :: String -> Parser ()
reserve str = (lexeme . try) (string' str *> notFollowedBy alphaNumChar)

symbol :: String -> Parser String
symbol = L.symbol space

paren = between (symbol "(") (symbol ")")


-- Context
type Context = [String]

mkContext :: Context
mkContext = []

bindVar :: String -> Context -> Context
bindVar = (:)

varAt :: Int -> Context -> String 
varAt n ctx = 
    if n < length ctx then ctx !! n else error "out of bound" 

indexOf :: String -> Context -> Maybe Int 
indexOf str [] = Nothing 
indexOf str (x:xs) = if x == str then Just 0 else (1+) <$> indexOf str xs 

-- Parser
-- | lambda ::= x                  variable
-- |            λx.t               abstraction
-- |            t t                application

data Term =
      Var Int                  -- de Brujin index
    | Abs Term 
    | App Term Term 
    deriving (Eq)

instance Show Term where 
    show term = 
        case term of 
            Var i  -> show i 
            Abs t -> "λ." ++ show t 
            App t1 t2 -> "(" ++ show t1 ++ " " ++ show t2 ++ ")"

identifier :: Parser String 
identifier = (lexeme . try) (p >>= check )
    where 
        p =  (:) <$> letterChar <*> many alphaNumChar
        check x = if map toLower x == "lambda"
                    then fail "keyword lambda cannot be a identifier"
                    else return x 

var :: Parser Term
var = do 
    id  <- identifier
    ctx <- get
    case indexOf id ctx of 
        Just i -> return $ Var i 
        _ -> lift . fail $ "unbinding variable `" ++ show id ++ "`"

abs :: Parser Term
abs = do 
    reserve "lambda"
    var <- identifier
    symbol "."
    ctx <- get             -- current context
    modify $ bindVar var   
    t <- _term
    put ctx                -- restore context
    return $ Abs t

app :: Parser Term 
app = paren (App <$> _term <*> _term)


_term :: Parser Term 
_term = try var <|> abs <|> app

term = between space eof _term


parseTerm s = evalState (runParserT term "test" s) mkContext