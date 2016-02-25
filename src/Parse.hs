module Parse where

import Prelude hiding (break, return)
import Control.Applicative
import Data.Char
import Rule
import Type
import Expr
import Pos
import Lex (Token(..))


-- Parse tree definitions
data Stmt
    = Decl Decl
    | Expr Expr
    | Assign Expr Expr
    | If Expr Block Block
    | While Expr Block
    | Return Expr
    | Break
    | Continue
    deriving Show

type Block = [(Pos, Stmt)]

data Decl
    = Def String Struct Struct Block
    | Let Expr Expr
    | Extern Expr
    | Import String
    deriving Show

type Tree = [(Pos, Decl)]


-- General token matching rules
symbol :: Rule (Pos, Token) String
symbol = rule $ (.map snd) $ \case
    Symbol _ s:_ -> accept 1 s
    _            -> reject

op :: Rule (Pos, Token) String
op = rule $ (.map snd) $ \case
    Op _ s:_ -> accept 1 s
    _        -> reject

int :: Rule (Pos, Token) Int
int = rule $ (.map snd) $ \case
    Int' i:_ -> accept 1 i
    _        -> reject

float :: Rule (Pos, Token) Double
float = rule $ (.map snd) $ \case
    Float' f:_ -> accept 1 f
    _          -> reject

string :: Rule (Pos, Token) String
string = rule $ (.map snd) $ \case
    String s:_ -> accept 1 s
    _          -> reject

term :: Rule (Pos, Token) Token
term = rule $ (.map snd) $ \case
    t@Term:_ -> accept 1 t
    _        -> reject

token :: String -> Rule (Pos, Token) Token
token s' = rule $ (.map snd) $ \case
    t@(Symbol _ s):_ | s == s' -> accept 1 t
    t@(Op _ s):_     | s == s' -> accept 1 t
    t@(Token s):_    | s == s' -> accept 1 t
    _                          -> reject


-- Parsing rules
type' :: Rule (Pos, Token) Type
type' = suffixM typeBase typeSuffix
  where
    typeBase = rule $ (.map snd) $ \case
        Token "(":_  -> fromStruct <$> struct
        Symbol _ s:_ -> accept 1 $ Type s
        _            -> reject

    typeSuffix t = rule $ (.map snd) $ \case
        Token "[":_ -> Array t <$ token "[" <*> optional int <* token "]"
        _           -> reject

entry :: Rule (Pos, Token) (Type, Maybe String)
entry = (,) <$> type' <*> optional symbol

struct :: Rule (Pos, Token) Struct
struct = rule $ (.map snd) $ \case
    Token "(":_ -> token "(" *> struct <* token ")"
    _           -> delimited entry (token ",")

import' :: Rule (Pos, Token) Decl
import' = Import
  <$  token "import"
  <*> symbol 

def :: Rule (Pos, Token) Decl
def = Def
  <$  token "def"
  <*> (symbol <|> op)
  <*> struct <* token "->" <*> struct
  <*> block

let' :: Rule (Pos, Token) Decl
let' = Let
  <$  token "let"
  <*> expr <* token "=" <*> expr

extern :: Rule (Pos, Token) Decl
extern = Extern
  <$  token "extern"
  <*> expr

decl :: Rule (Pos, Token) Decl
decl = rule $ (.map snd) $ \case
    Symbol _ "import":_ -> import'
    Symbol _ "def":_    -> def
    Symbol _ "let":_    -> let'
    Symbol _ "extern":_ -> extern
    _                   -> reject

expr :: Rule (Pos, Token) Expr
expr = subExpr maxBound

subExpr :: Int -> Rule (Pos, Token) Expr
subExpr prec = suffixM preExpr postExpr
  where
    preExpr = rule $ (.map snd) $ \case
        Token "(":_   -> token "(" *> expr <* token ")"
        Op prec' s:_  -> Call (Var s) . pure <$ op <*> subExpr prec'
        Symbol _ s:_  -> accept 1 $ Var s
        Int' i:_      -> accept 1 $ Int i
        Float' f:_    -> accept 1 $ Float f
        String s:_    -> accept 1 $ Tuple (map (Int . ord) s)
        _             -> reject

    postExpr x = rule $ (.map snd) $ \case
        Token "(":_ -> Call x
          <$ token "(" <*> delimited expr (token ",") <* token ")"
        Token ".":_ -> Access x
          <$ token "." <*> symbol
        Op prec' s:_ | prec > prec' -> (\y -> Call (Var s) [x,y])
          <$ op <*> subExpr prec'
        _ -> reject

block :: Rule (Pos, Token) Block
block = token "{" *> (concat <$> separated nested term) <* token "}"
  where
    nested = rule $ (.map snd) $ \case
        Token "{":_ -> block
        _           -> pure <$> (pos <*> stmt)

if' :: Rule (Pos, Token) Stmt
if' = If
  <$  token "if" <* token "(" <*> expr <* token ")"
  <*> block
  <*> (token "else" *> block <|> pure [])

while :: Rule (Pos, Token) Stmt
while = While
  <$  token "while" <* token "(" <*> expr <* token ")"
  <*> block

return :: Rule (Pos, Token) Stmt
return = Return <$ token "return" <*> expr

break :: Rule (Pos, Token) Stmt
break = Break <$ token "break"

continue :: Rule (Pos, Token) Stmt
continue = Continue <$ token "continue"

assign :: Rule (Pos, Token) Stmt
assign = Assign <$> expr <* token "=" <*> expr

stmt :: Rule (Pos, Token) Stmt
stmt = rule $ (.map snd) $ \case
    Symbol _ "if":_       -> if'
    Symbol _ "while":_    -> while
    Symbol _ "return":_   -> return
    Symbol _ "break":_    -> break
    Symbol _ "continue":_ -> continue
    Symbol _ "let":_      -> Decl <$> let'
    _                     -> do
        e <- expr
        rule $ (.map snd) $ \case
            Token "=":_ -> Assign e <$ token "=" <*> expr
            _           -> Expr <$> pure e

tree :: Rule (Pos, Token) Tree
tree = separated (pos <*> decl) term


-- Parsing entry point
parse :: FilePath -> [(Pos, Token)] -> Result Tree
parse fp = expect fp . run tree

