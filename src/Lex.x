{
module Lex where

import           Data.Text (Text)
import           Prelude hiding (LT, GT)

import qualified Data.Text as T

}

%wrapper "posn"

$unicodeIds = $printable # [$white \.\;\'\"\(\)]
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
    $s $e $l $e $c $t { \_ _ -> Select }
    $d $e $l $e $t $e { \_ _ -> Delete }
    $f $r $o $m { \_ _ -> From }
    $w $h $e $r $e { \_ _ -> Where }
    "=" { \_ _ -> Equals }
    "<>" { \_ _ -> NotEquals }
    "!=" { \_ _ -> NotEquals }
    [\'] $quoted* [\'] { \_ -> String . T.pack . init . tail }
    $unicodeIds+ { \_ -> Name . T.pack }
    

{

data Token = Delete | Select | Insert
     | From | Where
     | Name Text | String Text
     | Equals | NotEquals
     deriving Show

}
