{
{-# LANGUAGE DuplicateRecordFields #-}
module Parser where

import qualified Lex as L
import Syntax
import Internal (mkName)

import           Data.List.NonEmpty        (NonEmpty (..))

import qualified Data.List.NonEmpty as NE
}

%name parse
%tokentype { L.Token }
%error { parseError }
%monad { Either String }

%token
    delete { L.Delete }
    select { L.Select }
    insert { L.Insert }

    from { L.From }
    where { L.Where }
    into { L.Into }
    values { L.Values }
    '(' { L.LParen }
    ')' { L.RParen }
    comma { L.Comma }

    name { L.Name $$ }
    string { L.String $$ }

    '=' { L.Equals }
    '!=' { L.NotEquals }

%%

Query
    : Delete { QD $1 }
    | Select { QS $1 }
    | Insert { QI $1 }

Delete
    : delete from Name where Condition { Delete $3 (Just $5) }
    | delete from Name { Delete $3 Nothing }

Select
    : select NameList from Name { Select { table = $4, columns = NE.fromList (reverse $2), conditions = [] } }

Insert : insert into Name '(' NameList ')' values '(' LitList ')'
       { Insert { table = $3, columns = NE.fromList (reverse $5), values = NE.fromList (reverse $9) } }

{- These lists are non-empty by construction, but not by type. List head is the right-most element. -}

NameList
    : Name { [$1] }
    | NameList comma Name { $3 : $1 }

ExprList
    : Expr { [$1] }
    | ExprList comma Expr { $3 : $1 }

LitList
    : Literal { [$1] }
    | LitList comma Literal { $3 : $1 }

Operator
    : '=' { Eq }
    | '!=' { NEq }

Condition : Name Operator Expr { Op $2 $1 $3 }

Name : name { mkName $1 }

Expr
    : Literal { Lit $1 }
    | Name { Var $1 }

Literal : string { T $1 }

{
parseError :: [L.Token] -> Either String a
parseError _ = Left "Parse error"
}
