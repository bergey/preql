{
module Parser where

import qualified Lex as L
import Syntax
import Internal (mkName)
}

%name parse
%tokentype { L.Token }
%error { parseError }

%token
    delete { L.Delete }
    from { L.From }
    name { L.Name $$ }
    select { L.Select }
    where { L.Where }
    '=' { L.Equals }
    string { L.String $$ }

%%

Query : Delete { QD $1 }

Delete
    : delete from Name where Condition { Delete $3 (Just $5) }
    | delete from Name { Delete $3 Nothing }

Condition : Name '=' Expr { Op Eq $1 $3 }

Name : name { mkName $1 }

Expr
    : string { Lit (T $1) }
    | Name { Var $1 }


{
parseError :: [L.Token] -> a
parseError _ = error "Parse error"
}
