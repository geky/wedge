module Parse where

import Prelude hiding (break, return)
import Control.Applicative
import Data.Char
import Rule
import Type
import Pos
import Lex (Token,Token'(..))


-- Parse tree definitions
data Expr
    = Call Expr [Expr]
    | Access Expr String
    | Var String
    | IntLit Int
    | FloatLit Double
    | ArrayLit [Expr]
    deriving Show

type Stmt = (Pos, Stmt')
data Stmt'
    = Decl Decl'
    | Expr Expr
    | Assign Expr Expr
    | If Expr [Stmt] [Stmt]
    | While Expr [Stmt]
    | Return Expr
    | Break
    | Continue
    deriving Show

type Decl = (Pos, Decl')
data Decl'
    = Let (Either Expr (Type, Maybe String)) (Maybe Expr)
    | Def Tuple (Maybe Tuple) String [Stmt]
    | Import String
    deriving Show

type Tree = [Decl]


-- General token matching rules
symbol :: Rule Token String
symbol = rule1 $ \((_,t):ts) -> case t of
    Symbol s -> accept s ts
    _        -> reject

op :: Rule Token String
op = rule1 $ \((_,t):ts) -> case t of
    Op s _ -> accept s ts
    _      -> reject

int :: Rule Token Int
int = rule1 $ \((_,t):ts) -> case t of
    Int i -> accept i ts
    _     -> reject

float :: Rule Token Double
float = rule1 $ \((_,t):ts) -> case t of
    Float f -> accept f ts
    _       -> reject

string :: Rule Token String
string = rule1 $ \((_,t):ts) -> case t of
    String s -> accept s ts
    _        -> reject

term :: Rule Token ()
term = rule1 $ \((_,t):ts) -> case t of
    Term -> accept () ts
    _    -> reject

token :: String -> Rule Token Token
token s' = rule $ \case
    t@(_, Symbol s):ts | s == s' -> accept t ts
    t@(_, Op s _):ts   | s == s' -> accept t ts
    t@(_, Token s):ts  | s == s' -> accept t ts
    _                            -> reject


-- Parsing rules
type' :: Rule Token Type
type' = suffix base baseSuffix
  where
    base = rule1 $ \((_,t):ts) -> case t of
        Symbol "void" -> accept Void ts
        Token "("     -> fromTuple <$> nested
        Token "{"     -> fromTuple <$> struct
        Symbol s      -> accept (Type s) ts
        _             -> reject

    baseSuffix = flip ArrayType <$ token "[" <*> optional int <* token "]"

    nested = token "(" *> suffix tuple nestSuffix <* token ")"
    nestSuffix = toFunc <$ token "->" <*> tuple
      where toFunc rets args = [(FuncType args rets, Nothing)]

struct, tuple :: Rule Token Tuple
struct = token "{" *> separated var term <* token "}"
tuple = rule1 $ \((_,t):_) -> case t of
    Token "(" -> token "(" *> tuple <* token ")"
    _         -> delimited var (token ",")

var :: Rule Token (Type, Maybe String)
var = (,) <$> type' <*> optional symbol

import' :: Rule Token Decl'
import' = Import
  <$ token "import" <*> symbol 

def :: Rule Token Decl'
def = (\name args rets -> Def args rets name)
  <$  token "def" <*> (symbol <|> op)
  <*> tuple <*> optional (token "->" *> tuple)
  <*> block

let' :: Rule Token Decl'
let' = Let
  <$> (   Left  <$  token "let" <*> expr 
      <|> Right <$> ((,) <$> type' <*> (Just <$> symbol)))
      -- TODO fix this in var
  <*> optional (token "=" *> expr)

decl :: Rule Token Decl
decl = rule1 $ \((p,t):_) -> (p,) <$> case t of
    Symbol "import" -> import'
    Symbol "def"    -> def
    _               -> let'

expr :: Rule Token Expr
expr = subExpr maxBound

subExpr :: Int -> Rule Token Expr
subExpr rec = suffix preExpr postExpr
  where
    preExpr = rule1 $ \((_,t):ts) -> case t of
        Token "(" -> token "(" *> expr <* token ")"
        Op s rec' -> Call (Var s) . pure <$ op <*> subExpr rec'
        Symbol s  -> accept (Var s) ts
        Int i     -> accept (IntLit i) ts
        Float f   -> accept (FloatLit f) ts
        String s  -> accept (ArrayLit $ map (IntLit . ord) s) ts
        _         -> reject

    postExpr = rule1 $ \((_,t):_) -> case t of
        Token "(" -> flip Call
          <$ token "(" <*> delimited expr (token ",") <* token ")"
        Token "." -> flip Access
          <$ token "." <*> symbol
        Op s rec' | rec > rec' -> (\b a -> Call (Var s) [a,b])
          <$ op <*> subExpr rec'
        _ -> reject

block :: Rule Token [Stmt]
block = concat <$ token "{" <*> separated nested term <* token "}"
  where
    nested = rule1 $ \((_,t):_) -> case t of
        Token "{" -> block
        _         -> pure <$> stmt

if' :: Rule Token Stmt'
if' = If
  <$  token "if" <* token "(" <*> expr <* token ")"
  <*> block
  <*> (token "else" *> block <|> pure [])

while :: Rule Token Stmt'
while = While
  <$  token "while" <* token "(" <*> expr <* token ")"
  <*> block

return :: Rule Token Stmt'
return = Return <$ token "return" <*> expr

break :: Rule Token Stmt'
break = Break <$ token "break"

continue :: Rule Token Stmt'
continue = Continue <$ token "continue"

assign :: Rule Token Stmt'
assign = Assign <$> expr <* token "=" <*> expr

stmt :: Rule Token Stmt
stmt = rule1 $ \((p,t):_) -> (p,) <$> case t of
    Symbol "if"       -> if'
    Symbol "while"    -> while
    Symbol "return"   -> return
    Symbol "break"    -> break
    Symbol "continue" -> continue
    _                 ->
          try (Decl <$> let' <* look term)
      <|> expr <**> (flip Assign <$ token "=" <*> expr <|> pure Expr)


-- Parsing entry oint
parse :: FilePath -> [Token] -> Tree
parse fp = run (separated decl term) (unexpected fp)

