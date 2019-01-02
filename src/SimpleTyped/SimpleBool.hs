module SimpleTyped.SimpleBool where 


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
type Context = [(String, Binding)]

data Binding =
      NameBind       -- for parsing & printting
    | VarBind Ty     -- for type checking
    deriving (Show, Eq)
 
data Ty = 
      TyBool        -- boolean type 
    | TyArr Ty Ty   -- function type
    deriving (Eq)

instance Show Ty where 
    show TyBool = "Bool"
    show (TyArr t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")" 

mkContext :: Context
mkContext = []

addBinding :: String -> Binding -> Context -> Context
addBinding var bind = ((var, bind):)

getTypeFromContext :: Context -> Int -> Either String Ty
getTypeFromContext [] i = Left "not found"
getTypeFromContext ctx i | i < 0 = Left "not found"
getTypeFromContext ((_, bind):_) 0 = 
    case bind of
        VarBind t -> Right t
        _         -> Left "not found"
getTypeFromContext (_:ctx) i = getTypeFromContext ctx (i-1)

indexOf :: String -> Context -> Maybe Int 
indexOf str [] = Nothing 
indexOf str ((x, _):xs) = if x == str then Just 0 else (1+) <$> indexOf str xs 

-- | Parser
-- lambda ::= x                  variable
--            λx.t               abstraction
--            t t                application
data Term =
      TTrue
    | TFalse
    | TIf Term Term Term
    | Var Int                    -- differ with books
    | Abs String Ty Term
    | App Term Term 
    deriving (Eq)

instance Show Term where 
    show term = 
        case term of 
            TTrue -> "True"
            TFalse -> "False"
            TIf t1 t2 t3 -> "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3 
            Var i  -> show i 
            Abs s ty t -> "λ" ++ s ++ ": " ++ show ty ++ "." ++ show t 
            App t1 t2 -> "(" ++ show t1 ++ " " ++ show t2 ++ ")"

identifier :: Parser String 
identifier = (lexeme . try) ((:) <$> letterChar <*> many alphaNumChar)

var :: Parser Term
var = do 
    id  <- identifier
    ctx <- get
    case indexOf id ctx of 
        Just i -> return $ Var i 
        _ -> lift . fail $ "unbinding variable `" ++ show id ++ "`"

parseBinding :: Parser Ty
parseBinding = (foldl1 TyArr) <$> (sepBy1 _type (symbol "->"))

_type = TyBool <$ reserve "Bool"
    <|> paren parseBinding 

abs :: Parser Term
abs = do 
    symbol "\\"
    var <- identifier
    symbol ":"
    binding <- parseBinding
    symbol "."
    ctx <- get             -- current context
    modify $ addBinding var (VarBind binding)
    t <- _term
    put ctx                -- restore context
    return $ Abs var binding t 

_if = do 
    reserve "if"
    t1 <- _term
    reserve "then"
    t2 <- _term
    reserve "else"
    t3 <- _term
    return $ TIf t1 t2 t3

app :: Parser Term 
app = paren (App <$> _term <*> _term)


bool :: Parser Term 
bool =  TTrue <$ reserve "true" 
    <|> TFalse <$ reserve "false"

_term :: Parser Term 
_term = bool <|>_if <|> try var <|> abs <|> app

term = between space eof _term

parseTerm s = evalState (runParserT term "test" s) mkContext

shift :: Int -> Term -> Term
shift = _shift 0

_shift :: Int -> Int -> Term -> Term
_shift c d term = 
    case term of 
        Var k -> Var $ if k < c then k else k + d
        Abs v ty t -> Abs v ty $ _shift (c+1) d t
        App t1 t2 -> App (_shift c d t1) (_shift c d t2)

-- [i -> s]t
subst :: Int -> Term -> Term -> Term
subst i s = _subst 0
    where _subst c t = 
            case t of
                Var k -> if k == i + c then shift c s else Var k
                Abs v ty t1 -> Abs v ty $ _subst (c+1) t1
                App t1 t2 -> App (_subst c t1) (_subst c t2)

-- λx.x λx.x => λx.x
-- λ.0  λ.0  => λ.0   
termSubstTop :: Term -> Term -> Term
termSubstTop v t = shift (-1) (subst 0 (shift 1 v) t) 


eval :: Context -> Term -> Term
eval ctx term = 
    case term of 
        App (Abs _ _ t1) v@(Abs _ _ t2) -> termSubstTop v t1
        App v@(Abs _ _ t1) t2 -> eval ctx (App v (eval ctx t2))
        App t1 t2 -> eval ctx (App (eval ctx t1) t2)
        _ -> term


typeOf :: Context -> Term -> Ty  
typeOf ctx t = 
    case t of 
        TTrue -> TyBool
        TFalse -> TyBool 
        TIf t1 t2 t3 -> if typeOf ctx t1 == TyBool 
                        then let ty2 = typeOf ctx t2 
                                 ty3 = typeOf ctx t3 
                             in if ty2 == ty3 
                                then ty2 
                                else error $ "then: " ++ show ty2 ++ "& else: " ++ show ty3 ++ " type mismatch "
                        else error $ "guard of conditional not a boolean"
        Var i -> either error id $ getTypeFromContext ctx i
        Abs v ty t -> 
            let ctx' = addBinding v (VarBind ty) ctx 
                ty2 = typeOf ctx' t 
            in TyArr ty ty2
        App t1 t2 -> 
            let ty1 = typeOf ctx t1 
                ty2 = typeOf ctx t2 
            in case ty1 of 
                TyArr a b -> if a == ty2 then b else error "parameter type mismatch"
                _         -> error "arrow type expected"