{
module Untyped.Lex where

import           Data.Text (Text)
import           Prelude hiding (LT, GT, lex)

import qualified Data.Text as T

}

%wrapper "monadUserState"

$unicodeIds = $printable # [$white \,\.\;\'\"\(\)\<\>=\+\-\^\!@]
$firstLetter = $unicodeIds # [0-9_\$]
$quoted = $printable # [\']
$digit = [0-9]
$haskell = $printable # [\}]
$a = [aA]
$b = [bB]
$c = [cC]
$d = [dD]
$e = [eE]
$f = [fF]
$g = [gG]
$h = [hH]
$i = [iI]
$j = [jJ]
$k = [kK]
$l = [lL]
$m = [mM]
$n = [nN]
$o = [oO]
$p = [pP]
$q = [qQ]
$r = [rR]
$s = [sS]
$t = [tT]
$u = [uU]
$v = [vV]
$w = [wW]
$x = [xX]
$y = [yY]
$z = [zZ]

tokens :-

    $white+            ;
    $a $s $c { lex Asc }
    $d $e $s $c { lex Desc }
    $o $r $d $e $r { lex Order }
    $b $y { lex By }
    $u $s $i $n $g { lex Using }
    $o $p $e $r $a $t $o $r { lex Operator }
    $u $n $i $o $n { lex Union }
    $e $x $c $e $p $t { lex Except }
    $n $u $l $l $s { lex Nulls }
    $f $i $r $s $t { lex First }
    $l $a $s $t { lex Last }
    $d $e $l $e $t $e { lex Delete }
    $s $e $l $e $c $t { lex Select }
    $i $n $s $e $r $t { lex Insert }
    $u $p $d $a $t $e { lex Update }
    $f $r $o $m { lex From }
    $w $h $e $r $e { lex Where }
    $i $n $t $o { lex Into }
    $v $a $l $u $e $s { lex Values }
    $s $e $t { lex Set }
    "(" { lex LParen }
    "," { lex Comma }
    ")" { lex RParen }
    "*" { lex Mul }
    "/" { lex Div }
    "+" { lex Add }
    "-" { lex Sub }
    "^" { lex Exponent }
    $i $s { lex Is }
    $n $o $t { lex Not }
    $n $u $l $l { lex Null }
    $i $s $n $u $l $l { lex IsNull }
    $n $o $t $n $u $l $l { lex NotNull }
    "<>" { lex NotEquals }
    "!=" { lex NotEquals }
    "<" { lex LT }
    "<=" { lex LTE }
    ">" { lex GT }
    ">=" { lex GTE }
    "=" { lex Equals }
    $l $i $k $e { lex Like }
    $i $l $i $k $e { lex ILike }
    $a $n $d { lex And }
    $o $r { lex Or }
    [\'] ("''" | $quoted)* [\'] { lex' (String . T.pack . unquoteString) }
    $firstLetter $unicodeIds* { lex' (Name . T.pack) }
    "-"? $digit+ ("." $digit+)? ($e "-"? $digit+)? { lex' (Number . read) }
    "$" $digit+ { lex' (NumberedParam . read . tail) }
    "${" $haskell+ "}" { lex' (HaskellParam . T.pack . init . drop 2) }
    ";" { lex Semicolon }

{

data LocToken = LocToken
     { loc :: AlexPosn
     , unLoc :: Token
     } deriving Show

data Token = Delete | Select | Insert | Update
    | Asc | Desc | Order | By | Using | Operator | Nulls | First | Last
    | Union | Except
     | From | Where | Into | Values | Set
     | Name Text | String Text | Number Double
     | NumberedParam Word | HaskellParam Text
     | LParen | RParen | Comma
     | Mul | Div | Add | Sub | Mod | Exponent
     | Is | Null | IsNull | NotNull
     | Equals | NotEquals | LT | LTE | GT | GTE
     | Like | ILike
     | And | Or | Not
     | Semicolon | EOF
     deriving (Show, Eq, Ord)

/* from https://github.com/dagit/happy-plus-alex/blob/master/src/Lexer.x */

-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = filePath <$> alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

-- For nice parser error messages.
unLex :: Token -> String
unLex t = case t of
    Asc -> "ASC"
    Desc -> "DESC"
    Order -> "ORDER"
    By -> "BY"
    Using -> "USING"
    Operator -> "OPERATOR"
    Union -> "UNION"
    Except -> "EXCEPT"
    Delete -> "DELETE"
    Select  -> "SELECT "
    Insert -> "INSERT"
    Update -> "UPDATE"
    From  -> "FROM"
    Where  -> "WHERE"
    Into  -> "INTO"
    Values -> "VALUES"
    Set -> "SET"
    Name n -> T.unpack n 
    String s -> T.unpack s
    Number n -> show n
    NumberedParam i -> '$' : show i
    HaskellParam s -> "${" ++ T.unpack s ++ "}"
    LParen  -> "("
    RParen  -> ")"
    Comma -> ","
    Mul -> "*"
    Div -> "/"
    Add -> "+"
    Sub -> "-"
    Exponent -> "^"
    Is -> "IS"
    Null -> "NULL"
    IsNull -> "IsNull"
    NotNull -> "NotNull"
    Equals -> "="
    NotEquals -> "!="
    Like -> "LIKE"
    ILike -> "ILIKE"
    And  -> "AND "
    Or -> "OR"
    Not -> "NOT"
    Semicolon -> ";"
    EOF -> "<EOF>"

-- | remove single quotes, and '' escape sequences
unquoteString :: String -> String
unquoteString ('\'' : rest) = go rest
  where
    go  ('\'' : '\'' : rest) = '\'' : go rest
    go ['\''] = ""
    go [_] = error "string did not end with a '"
    go ('\'' : _rest) = error "unescaped ' in middle"
    go (c : rest)  = c : go rest
unquoteString _ = error "string did not begin with a '"

alexEOF :: Alex LocToken
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ LocToken p EOF

-- Unfortunately, we have to extract the matching bit of string ourselves...
lex' :: (String -> Token) -> AlexAction LocToken
lex' f = \(p,_,_,s) i -> return $ LocToken p (f (take i s))

-- For constructing tokens that do not depend on the input
lex :: Token -> AlexAction LocToken
lex = lex' . const

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex LocToken
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexErrorPosn p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexErrorPosn :: AlexPosn -> String -> Alex a
alexErrorPosn (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlexWithFilepath :: Alex a -> FilePath -> String -> Either String a
runAlexWithFilepath a fp input = runAlex input (setFilePath fp >> a)

lexAll :: Alex [LocToken]
lexAll = do
    token <- alexMonadScan
    case unLoc token of
        EOF -> return [token]
        _ -> fmap (token :) lexAll

testLex' :: String -> Either String [LocToken]
testLex' s = runAlex s lexAll

testLex :: String -> Either String [Token]
testLex s = map unLoc <$> testLex' s

}
