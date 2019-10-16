{
module Lex where

import           Data.Text (Text)
import           Prelude hiding (LT, GT)

import qualified Data.Text as T

}

%wrapper "posn"

$unicodeIds = $printable # [$white \.\;\'\"\(\)]
$quoted = $printable # [\']

tokens :-

    $white+            ;
    "SELECT" { \_ _ -> Select }
    "DELETE" { \_ _ -> Delete }
    "FROM" { \_ _ -> From }
    "WHERE" { \_ _ -> Where }
    "=" { \_ _ -> Equals }
    [\'] $quoted* [\'] { \_ -> String . T.pack . init . tail }
    $unicodeIds+ { \_ -> Name . T.pack }
    

{

data Token = Delete | From | Name Text | Select | Where | Equals | String Text
     deriving Show

}
