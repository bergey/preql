{
module Lex where

import           Data.Text (Text)
import           Prelude hiding (LT, GT, lex)

import qualified Data.Text as T

}

%wrapper "monadUserState"

$unicodeIds = $printable # [$white \,\.\;\'\"\(\)\<\>=\+\-\^\!@]
$firstLetter = $unicodeIds # [0-9_\$]
$quoted = $printable # [\']
$digit = [0-9]
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
    $d $e $l $e $t $e { lex Delete }
    $s $e $l $e $c $t { lex Select }
    $i $n $s $e $r $t { lex Insert }
    $f $r $o $m { lex From }
    $w $h $e $r $e { lex Where }
    $i $n $t $o { lex Into }
    $v $a $l $u $e $s { lex Values }
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
    "=" { lex Equals }
    "<>" { lex NotEquals }
    "!=" { lex NotEquals }
    "<" { lex LT }
    "<=" { lex LTE }
    ">" { lex GT }
    "<=" { lex GTE }
    $l $i $k $e { lex Like }
    $i $l $i $k $e { lex ILike }
    $a $n $d { lex And }
    $o $r { lex Or }
    [\'] $quoted* [\'] { lex' (String . T.pack . init . tail) }
    $firstLetter $unicodeIds* { lex' (Name . T.pack) }
    $digit+ { lex' (Number . read) }
    $digit+ "." $digit+ { lex' (Number . read) }
    

{

data LocToken = LocToken AlexPosn Token
     deriving Show

data Token = Delete | Select | Insert
     | From | Where | Into | Values
     | Name Text | String Text | Number Double
     | LParen | RParen | Comma
     | Mul | Div | Add | Sub | Exponent
     | Is | Null | IsNull | NotNull
     | Equals | NotEquals | LT | LTE | GT | GTE
     | Like | ILike
     | And | Or | Not
     | EOF
     deriving Show

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
    Delete -> "DELETE"
    Select  -> "SELECT "
    Insert -> "INSERT"
    From  -> "FROM "
    Where  -> "WHERE "
    Into  -> "INTO "
    Values -> "VALUES"
    Name n -> T.unpack n 
    String s -> T.unpack s
    Number n -> show n
    LParen  -> "("
    RParen  -> ")"
    Comma -> ","
    Equals -> "="
    NotEquals -> "!="
    And  -> "AND "
    Or -> "OR"

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
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)

}
