{
{-# LANGUAGE DuplicateRecordFields #-}
module Untyped.Parser where

import Untyped.Syntax
import Untyped.Name (mkName)
import Untyped.Lex (Alex, LocToken(..), Token)

import           Prelude hiding (LT, GT, lex)
import           Data.List.NonEmpty        (NonEmpty (..))

import qualified Untyped.Lex as L
import qualified Data.List.NonEmpty as NE
}

%name parseQuery_ Query
%name parseCondition_ Condition
%name parseExpr_ Expr
%tokentype { L.LocToken }
%monad { Alex }
%lexer { lexwrap } {  L.LocToken _ L.EOF }
%error { happyError }

%left or
%left and
%right not
%right '='
%left '<' '>'
%nonassoc like ilike
%left '!=' '<=' '>='
%nonassoc notnull
%nonassoc isnull
%nonassoc is
%left '+' '-'
%left '*' '/'
%left '^'

%token
    DELETE { LocToken _ L.Delete }
    SELECT { LocToken _ L.Select }
    INSERT { LocToken _ L.Insert }
    UPDATE { LocToken _ L.Update }

    FROM { LocToken _ L.From }
    WHERE { LocToken _ L.Where }
    INTO { LocToken _ L.Into }
    VALUES { LocToken _ L.Values }
    SET { LocToken _ L.Set }
    '(' { LocToken _ L.LParen }
    ')' { LocToken _ L.RParen }
    COMMA { LocToken _ L.Comma }

    NAME { LocToken _ (L.Name $$) }
    STRING { LocToken _ (L.String $$) }
    NUMBER { LocToken _ (L.Number $$) }
    PARAM { LocToken _ (L.NumberedParam $$) }
    HASKELL_PARAM { LocToken _ (L.HaskellParam $$) }

    '*' { LocToken _ L.Mul }
    '/' { LocToken _ L.Div }
    '+' { LocToken _ L.Add }
    '-' { LocToken _ L.Sub }
    '^' { LocToken _ L.Exponent }

    IS { LocToken _ L.Is }
    NULL { LocToken _ L.Null }
    ISNULL { LocToken _ L.IsNull }
    NOTNULL { LocToken _ L.NotNull }

    '=' { LocToken _ L.Equals }
    '!=' { LocToken _ L.NotEquals }
    '<' { L.LocToken _ L.LT }
    '>' { L.LocToken _ L.GT }
    '<=' { L.LocToken _ L.LTE }
    '>=' { L.LocToken _ L.GTE }
    NOT { L.LocToken _ L.Not }
    LIKE { L.LocToken _ L.Like }
    ILIKE { L.LocToken _ L.ILike }

    AND  { LocToken _ L.And }
    OR { LocToken _ L.Or }

    SEMICOLON { LocToken _ L.Semicolon }

%%

Query :: { Query }
    : Query1 { $1 }
    | Query1 SEMICOLON { $1 }

Query1 :: { Query }
    : Delete { QD $1 }
    | Select { QS $1 }
    | Insert { QI $1 }
    | Update { QU $1 }

Delete
    : DELETE FROM Name WHERE Condition { Delete $3 (Just $5) }
    | DELETE FROM Name { Delete $3 Nothing }

Select
    : SELECT ExprList FROM Name WHERE Condition { Select { table = $4, columns = NE.fromList (reverse $2), conditions = Just $6 } }
    | SELECT ExprList FROM Name { Select { table = $4, columns = NE.fromList (reverse $2), conditions = Nothing } }

Insert : INSERT INTO Name '(' NameList ')' VALUES '(' ExprList ')'
       { Insert { table = $3, columns = NE.fromList (reverse $5), values = NE.fromList (reverse $9) } }

Update :: { Update }
    : UPDATE Name SET SettingList WHERE Condition { Update { table = $2, settings = NE.fromList (reverse $4), conditions = Just $6 } }
    | UPDATE Name SET SettingList { Update { table = $2, settings = NE.fromList (reverse $4), conditions = Nothing } }

{- These lists are non-empty by construction, but not by type. List head is the right-most element. -}

list(el)
    : el { [$1] }
    | list(el) COMMA el { $3 : $1 }

NameList : list(Name) { $1 }

ExprList : list(Expr) { $1 }

SettingList : list(Setting) { $1 }

Compare :: { Compare }
    : '=' { Eq }
    | '!=' { NEq }
    | '<' { LT }
    | '>' { GT }
    | '<=' { LTE }
    | '>=' { GTE }
    | LIKE { Like }
    | ILIKE { ILike }

Condition
    : Name Compare Expr { Compare $2 $1 $3 }
    | Condition AND Condition { And $1 $3 }
    | Condition OR Condition { Or $1 $3 }
    | NOT Condition { Not $2 }
    | '(' Condition ')' { $2 }

Setting :: { Setting }
    : Name '=' Expr { Setting $1 $3 }

Name : NAME { mkName $1 }

Expr :: { Expr }
    : Literal { Lit $1 }
    | Name { Var $1 }
    | PARAM { NumberedParam $1 }
    | HASKELL_PARAM { HaskellParam $1 }
    | '(' Expr ')' { $2 }
    | Expr '^' Expr { BinOp Exponent $1 $3 }
    | Expr '*' Expr { BinOp Mul $1 $3 }
    | Expr '/' Expr { BinOp Div $1 $3 }
    | Expr '+' Expr { BinOp Add $1 $3 }
    | Expr '-' Expr { BinOp Sub $1 $3 }
    | Expr '=' Expr { BinOp (Comp  Eq) $1 $3 }
    | Expr '!=' Expr { BinOp (Comp  NEq) $1 $3 }
    | Expr '<' Expr { BinOp (Comp  LT) $1 $3 }
    | Expr '>' Expr { BinOp (Comp  GT) $1 $3 }
    | Expr '<=' Expr { BinOp (Comp  LTE) $1 $3 }
    | Expr '>=' Expr { BinOp (Comp  GTE) $1 $3 }
    | Expr LIKE Expr { BinOp (Comp  Like) $1 $3 }
    | Expr ILIKE Expr { BinOp (Comp  ILike) $1 $3 }
    | NOT Expr { Unary NegateBool $2 }
    | '-' Expr { Unary NegateNum $2 }
    | Expr Null { Unary $2 $1 }

Literal
        : STRING { T $1 }
        | NUMBER { F $1 }

Null
        : IS NULL { IsNull }
        | ISNULL { IsNull }
        | IS NOT NULL { NotNull }
        | NOTNULL { NotNull }


{

-- from https://github.com/dagit/happy-plus-alex/blob/master/src/Parser.y

lexwrap :: (L.LocToken -> Alex a) -> Alex a
lexwrap = (L.alexMonadScan' >>=)

happyError :: L.LocToken -> Alex a
happyError (L.LocToken p t) =
  L.alexError' p ("parse error at token '" ++ L.unLex t ++ "'")

parseQuery :: FilePath -> String -> Either String Query
parseQuery = L.runAlex' parseQuery_

parseCondition :: FilePath -> String -> Either String Condition
parseCondition = L.runAlex' parseCondition_

parseExpr :: FilePath -> String -> Either String Expr
parseExpr = L.runAlex' parseExpr_

}
