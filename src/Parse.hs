module Parse where

import Prelude hiding (break, return)
import Control.Applicative
import Data.Char
import Rule
import Type
import File
import Lex (Token(..))


-- Parse tree definitions
data Expr
    = Call Expr [Expr]
    | Access Expr String
    | Var String
    | IntLit Int
    | FloatLit Double
    | ArrayLit [Expr]
    deriving Show

data Stmt
    = Decl Decl             Pos
    | Expr Expr             Pos
    | Assign Expr Expr      Pos
    | If Expr [Stmt] [Stmt] Pos
    | While Expr [Stmt]     Pos
    | Return Expr           Pos
    | Break                 Pos
    | Continue              Pos
    deriving Show

data Decl
    = Let (Either Expr (Type, Maybe String)) (Maybe Expr) Pos
    | Def Tuple (Maybe Tuple) String [Stmt]               Pos
    | Import String                                       Pos
    deriving Show

type Tree = [Decl]

--
--instance Positional Expr where
--    getPos (Call _ _   p) = p
--    getPos (Access _ _ p) = p
--    getPos (Var _      p) = p
--    getPos (IntLit _   p) = p
--    getPos (FloatLit _ p) = p
--    getPos (ArrayLit _ p) = p
--
instance Positional Stmt where
    getPos (Decl _     p) = p
    getPos (Expr _     p) = p
    getPos (Assign _ _ p) = p
    getPos (If _ _ _   p) = p
    getPos (While _ _  p) = p
    getPos (Return _   p) = p
    getPos (Break      p) = p
    getPos (Continue   p) = p

instance Positional Decl where
    getPos (Let _ _     p) = p
    getPos (Def _ _ _ _ p) = p
    getPos (Import _    p) = p


-- General token matching rules
symbol :: Rule Token String
symbol = rule $ \case
    Symbol s _:ts -> accept s ts
    _             -> reject

op :: Rule Token String
op = rule $ \case
    Op s _ _:ts -> accept s ts
    _           -> reject

int :: Rule Token Int
int = rule $ \case
    Int i _:ts -> accept i ts
    _          -> reject

float :: Rule Token Double
float = rule $ \case
    Float f _:ts -> accept f ts
    _            -> reject

string :: Rule Token String
string = rule $ \case
    String s _:ts -> accept s ts
    _             -> reject

term :: Rule Token ()
term = rule $ \case
    Term _:ts -> accept () ts
    _         -> reject

token :: String -> Rule Token Token
token s' = rule $ \case
    t@(Symbol s _):ts | s == s' -> accept t ts
    t@(Op s _ _):ts   | s == s' -> accept t ts
    t@(Token s _):ts  | s == s' -> accept t ts
    _                           -> reject


-- Parsing rules
type' :: Rule Token Type
type' = suffix base baseSuffix
  where
    base = rule $ \case
        Symbol "void" _:ts -> accept Void ts
        Token "(" _:_      -> fromTuple <$> nested
        Token "{" _:_      -> fromTuple <$> struct
        Symbol s _:ts      -> accept (Type s) ts
        _                  -> reject

    baseSuffix = flip ArrayType <$ token "[" <*> optional int <* token "]"

    nested = token "(" *> suffix tuple nestSuffix <* token ")"
    nestSuffix = toFunc <$ token "->" <*> tuple
      where toFunc rets args = [(FuncType args rets, Nothing)]

struct, tuple :: Rule Token Tuple
struct = token "{" *> separated var term <* token "}"
tuple = rule $ \case
    Token "(" _:_ -> token "(" *> tuple <* token ")"
    _             -> delimited var (token ",")

var :: Rule Token (Type, Maybe String)
var = (,) <$> type' <*> optional symbol

import' :: Rule Token (Pos -> Decl)
import' = Import
  <$ token "import" <*> symbol 

def :: Rule Token (Pos -> Decl)
def = (\name args rets -> Def args rets name)
  <$  token "def" <*> (symbol <|> op)
  <*> tuple <*> optional (token "->" *> tuple)
  <*> block

let' :: Rule Token (Pos -> Decl)
let' = Let
  <$> (   Left  <$  token "let" <*> expr 
      <|> Right <$> ((,) <$> type' <*> (Just <$> symbol)))
      -- TODO fix this in var
  <*> optional (token "=" *> expr)

decl :: Rule Token Decl
decl = propagate getPos $ rule $ \case
    Symbol "import" _:_ -> import'
    Symbol "def" _:_    -> def
    _                   -> let'

expr :: Rule Token Expr
expr = subExpr maxBound

subExpr :: Int -> Rule Token Expr
subExpr rec = suffix preExpr postExpr
  where
    preExpr = rule $ \case
        Token "(" _:_  -> token "(" *> expr <* token ")"
        Op s rec' _:_  -> Call (Var s) . pure <$ op <*> subExpr rec'
        Symbol s _:ts  -> accept (Var s) ts
        Int i _:ts     -> accept (IntLit i) ts
        Float f _:ts   -> accept (FloatLit f) ts
        String s _:ts  -> accept (ArrayLit $ map (IntLit . ord) s) ts
        _              -> reject

    postExpr = rule $ \case
        Token "(" _:_ -> flip Call
          <$  token "("
          <*> delimited expr (token ",")
          <*  token ")"
        Token "." _:_ -> flip Access
          <$  token "." <*> symbol
        Op s rec' _:_ | rec > rec' -> (\b a -> Call (Var s) [a,b])
          <$  op <*> subExpr rec'
        _            -> reject

block :: Rule Token [Stmt]
block = concat <$ token "{" <*> separated nested term <* token "}"
  where
    nested = rule $ \case
        Token "{" _:_ -> block
        _             -> pure <$> stmt

if' :: Rule Token (Pos -> Stmt)
if' = If 
  <$  token "if" <* token "(" <*> expr <* token ")"
  <*> block
  <*> (token "else" *> block <|> pure [])

while :: Rule Token (Pos -> Stmt)
while = While
  <$  token "while" <* token "(" <*> expr <* token ")"
  <*> block

return :: Rule Token (Pos -> Stmt)
return = Return <$ token "return" <*> expr

break :: Rule Token (Pos -> Stmt)
break = Break <$ token "break"

continue :: Rule Token (Pos -> Stmt)
continue = Continue <$ token "continue"

assign :: Rule Token (Pos -> Stmt)
assign = Assign <$> expr <* token "=" <*> expr

stmt :: Rule Token Stmt
stmt = propagate getPos $ rule $ \case
    Symbol "if" _:_       -> if'
    Symbol "while" _:_    -> while
    Symbol "return" _:_   -> return
    Symbol "break" _:_    -> break
    Symbol "continue" _:_ -> continue
    _                     ->
          try ((\l p -> Decl (l p) p) <$> let' <* look term)
      <|> expr <**> (flip Assign <$ token "=" <*> expr <|> pure Expr)


-- Parsing entry oint
parse :: FilePath -> [Token] -> Tree
parse fp = run (separated decl term) err
  where err = unexpected fp show

