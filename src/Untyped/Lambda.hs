module Untyped.Lambda (Term(..), shift, term, parseTerm, termSubstTop, eval, mkContext) where 
    

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


-- Subsitution:
-- | [x -> s] x = s
-- | [x -> s] y = y           if x != y
-- | [x -> s] λy.t = λy.[x -> s]t
-- | [x -> s] (t1 t2) = ([x -> s]t1 [x -> s] t2)
-- De Index
-- out contxt  [s: (z λw.w), ?, ?, z]
-- | [1 -> s] λ.2  i.e. [x -> s] λy.x
-- shift bound var

-- shift(c, d) (k) = k if k < c else k + d 
--             (λ.t) = λ.shift(c+1,d)(t)
--             ((t1 t2))  = (shift(c, d)(t1) (shift(c, d)(t2))

shift :: Int -> Term -> Term
shift = _shift 0

_shift :: Int -> Int -> Term -> Term
_shift c d term = 
    case term of 
        Var k -> Var $ if k < c then k else k + d
        Abs t -> Abs $ _shift (c+1) d t
        App t1 t2 -> App (_shift c d t1) (_shift c d t2)

-- [i -> s]t
subst :: Int -> Term -> Term -> Term
subst i s t = _subst 0 t
    where _subst c t = 
            case t of
                Var k -> if k == i + c then shift c s else Var k
                Abs t1 -> Abs $ _subst (c+1) t1
                App t1 t2 -> App (_subst c t1) (_subst c t2)

-- λx.x λx.x => λx.x
-- λ.0  λ.0  => λ.0   
termSubstTop :: Term -> Term -> Term
termSubstTop v t = shift (-1) (subst 0 (shift 1 v) t) 


eval :: Context -> Term -> Term
eval ctx term = 
    case term of 
        App (Abs t1) v@(Abs t2) -> termSubstTop v t1
        App v@(Abs t1) t2 -> eval ctx (App v (eval ctx t2))
        App t1 t2 -> eval ctx (App (eval ctx t1) t2)
        _ -> term
