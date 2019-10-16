{
module Lex where

import           Data.Text (Text)
import           Prelude hiding (LT, GT)

import qualified Data.Text as T

}

%wrapper "basic"

$unicodeIds = $printable # [$white \.\;\'\"\(\)]

tokens :-

    $white+            ;
    "SELECT" { \_ -> Select }
    "DELETE" { \_ -> Delete }
    "FROM" { \_ -> From }
    $unicodeIds+ { Name . T.pack }
    

{

data Token = Delete | From | Name Text | Select
     deriving Show

}
