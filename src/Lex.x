{
module Lex where

import           Data.Text (Text)
import           Prelude hiding (LT, GT, lex)

import qualified Data.Text as T

}

%wrapper "monadUserState"

$unicodeIds = $printable # [$white \,\.\;\'\"\(\)]
$quoted = $printable # [\']
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
    "=" { lex Equals }
    "<>" { lex NotEquals }
    "!=" { lex NotEquals }
    $a $n $d { lex And }
    $o $r { lex Or }
    [\'] $quoted* [\'] { lex' (String . T.pack . init . tail) }
    $unicodeIds+ { lex' (Name . T.pack) }
    

{

data LocToken = LocToken AlexPosn Token
     deriving Show

data Token = Delete | Select | Insert
     | From | Where | Into | Values
     | Name Text | String Text
     | LParen | RParen | Comma
     | Equals | NotEquals
     | And | Or
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