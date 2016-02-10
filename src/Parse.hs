module Parse where

import Prelude hiding (break, return)
import Control.Applicative
import Data.Char
import Rule
import Type
import Expr
import Result
import Pos
import Lex (Token, Token'(..))


-- Parse tree definitions
type Stmt = Positional Stmt'
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

type Decl = Positional Decl'
data Decl'
    = Let (Either Expr (Type, Maybe String)) (Maybe Expr)
    | Def Tuple (Maybe Tuple) String [Stmt]
    | Import String
    deriving Show

type Tree = [Decl]


-- General token matching rules
symbol :: Rule s Token' String
symbol = rule $ \case
    Symbol s:_ -> accept 1 s
    _          -> reject

op :: Rule s Token' String
op = rule $ \case
    Op s _:_ -> accept 1 s
    _        -> reject

int :: Rule s Token' Int
int = rule $ \case
    Int' i:_ -> accept 1 i
    _        -> reject

float :: Rule s Token' Double
float = rule $ \case
    Float' f:_ -> accept 1 f
    _          -> reject

string :: Rule s Token' String
string = rule $ \case
    String s:_ -> accept 1 s
    _          -> reject

term :: Rule s Token' Token'
term = rule $ \case
    t@Term:_ -> accept 1 t
    _        -> reject

token :: String -> Rule s Token' Token'
token s' = rule $ \case
    t@(Symbol s):_ | s == s' -> accept 1 t
    t@(Op s _):_   | s == s' -> accept 1 t
    t@(Token s):_  | s == s' -> accept 1 t
    _                        -> reject


-- Parsing rules
type' :: Rule Pos Token' Type
type' = suffix base baseSuffix
  where
    base = rule $ \case
        Token "(":_     -> fromTuple <$> nested
        Token "{":_     -> fromTuple <$> struct
        Symbol "void":_ -> accept 1 $ Void
        Symbol s:_      -> accept 1 $ Type s
        _               -> reject

    baseSuffix = flip ArrayType <$ token "[" <*> optional int <* token "]"

    nested = token "(" *> suffix tuple nestSuffix <* token ")"
    nestSuffix = toFunc <$ token "->" <*> tuple
      where toFunc rets args = [(FuncType args rets, Nothing)]

struct, tuple :: Rule Pos Token' Tuple
struct = token "{" *> separated var term <* token "}"
tuple = rule $ \case
    Token "(":_ -> token "(" *> tuple <* token ")"
    _           -> delimited var (token ",")

var :: Rule Pos Token' (Type, Maybe String)
var = (,) <$> type' <*> optional symbol

import' :: Rule Pos Token' Decl'
import' = Import
  <$ token "import" <*> symbol 

def :: Rule Pos Token' Decl'
def = (\name args rets -> Def args rets name)
  <$  token "def" <*> (symbol <|> op)
  <*> tuple <*> optional (token "->" *> tuple)
  <*> block

let' :: Rule Pos Token' Decl'
let' = Let
  <$> (   Left  <$  token "let" <*> expr 
      <|> Right <$> ((,) <$> type' <*> (Just <$> symbol)))
      -- TODO fix this in var
  <*> optional (token "=" *> expr)

decl :: Rule Pos Token' Decl
decl = at $ rule $ \case
    Symbol "import":_ -> import'
    Symbol "def":_    -> def
    _                 -> let'

expr :: Rule Pos Token' Expr
expr = subExpr maxBound

subExpr :: Int -> Rule Pos Token' Expr
subExpr rec = suffix preExpr postExpr
  where
    preExpr = rule $ \case
        Token "(":_ -> token "(" *> expr <* token ")"
        Op s rec':_ -> Call (Var s) . pure <$ op <*> subExpr rec'
        Symbol s:_  -> accept 1 $ Var s
        Int' i:_    -> accept 1 $ Int i
        Float' f:_  -> accept 1 $ Float f
        String s:_  -> accept 1 $ Array $ map (Int . ord) s
        _           -> reject

    postExpr = rule $ \case
        Token "(":_ -> flip Call
          <$ token "(" <*> delimited expr (token ",") <* token ")"
        Token ".":_ -> flip Access
          <$ token "." <*> symbol
        Op s rec':_ | rec > rec' -> (\b a -> Call (Var s) [a,b])
          <$ op <*> subExpr rec'
        _ -> reject

block :: Rule Pos Token' [Stmt]
block = concat <$ token "{" <*> separated nested term <* token "}"
  where
    nested = rule $ \case
        Token "{":_ -> block
        _           -> pure <$> stmt

if' :: Rule Pos Token' Stmt'
if' = If
  <$  token "if" <* token "(" <*> expr <* token ")"
  <*> block
  <*> (token "else" *> block <|> pure [])

while :: Rule Pos Token' Stmt'
while = While
  <$  token "while" <* token "(" <*> expr <* token ")"
  <*> block

return :: Rule Pos Token' Stmt'
return = Return <$ token "return" <*> expr

break :: Rule Pos Token' Stmt'
break = Break <$ token "break"

continue :: Rule Pos Token' Stmt'
continue = Continue <$ token "continue"

assign :: Rule Pos Token' Stmt'
assign = Assign <$> expr <* token "=" <*> expr

stmt :: Rule Pos Token' Stmt
stmt = at $ rule $ \case
    Symbol "if":_       -> if'
    Symbol "while":_    -> while
    Symbol "return":_   -> return
    Symbol "break":_    -> break
    Symbol "continue":_ -> continue
    _                   ->
          try (Decl <$> let' <* look term)
      <|> expr <**> (flip Assign <$ token "=" <*> expr <|> pure Expr)


-- Parsing entry point
parse :: FilePath -> [Token] -> Result (Positional String) Tree
parse fp = expect fp . run (separated decl term)

