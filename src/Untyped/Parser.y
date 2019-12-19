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

    ASC { LocToken _ L.Asc }
    DESC { LocToken _ L.Desc }
    ORDER { LocToken _ L.Order }
    BY { LocToken _ L.By }

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

SelectStmt :: { SelectStmt }
    : select_no_parens { $1 }
    | select_with_parens { $1 }

select_with_parens
    : '(' select_no_parens ')' { $2 }
    | '(' select_with_parens ')' { $2 }

select_no_parens :: { SelectStmt }
    : simple_select { SimpleSelect $1 }
    | select_clause sort_clause { SortedSelect $1 $2 }
--                {
--                    insertSelectOptions((SelectStmt *) $1, $2, NIL,
--                                        NULL, NULL, NULL,
--                                        yyscanner);
--                    $$ = $1;
--                }
--            | select_clause opt_sort_clause for_locking_clause opt_select_limit
--                {
--                    insertSelectOptions((SelectStmt *) $1, $2, $3,
--                                        list_nth($4, 0), list_nth($4, 1),
--                                        NULL,
--                                        yyscanner);
--                    $$ = $1;
--                }
--            | select_clause opt_sort_clause select_limit opt_for_locking_clause
--                {
--                    insertSelectOptions((SelectStmt *) $1, $2, $4,
--                                        list_nth($3, 0), list_nth($3, 1),
--                                        NULL,
--                                        yyscanner);
--                    $$ = $1;
--                }
--            | with_clause select_clause
--                {
--                    insertSelectOptions((SelectStmt *) $2, NULL, NIL,
--                                        NULL, NULL,
--                                        $1,
--                                        yyscanner);
--                    $$ = $2;
--                }
--            | with_clause select_clause sort_clause
--                {
--                    insertSelectOptions((SelectStmt *) $2, $3, NIL,
--                                        NULL, NULL,
--                                        $1,
--                                        yyscanner);
--                    $$ = $2;
--                }
--            | with_clause select_clause opt_sort_clause for_locking_clause opt_select_limit
--                {
--                    insertSelectOptions((SelectStmt *) $2, $3, $4,
--                                        list_nth($5, 0), list_nth($5, 1),
--                                        $1,
--                                        yyscanner);
--                    $$ = $2;
--                }
--            | with_clause select_clause opt_sort_clause select_limit opt_for_locking_clause
--                {
--                    insertSelectOptions((SelectStmt *) $2, $3, $5,
--                                        list_nth($4, 0), list_nth($4, 1),
--                                        $1,
--                                        yyscanner);
--                    $$ = $2;
--                }
--        ;

select_clause :: { SelectStmt }
    : simple_select                            { SimpleSelect $1 }
    | select_with_parens                    { $1 }

simple_select :: { SimpleSelect }
--            : SELECT opt_all_clause opt_target_list
--            into_clause from_clause where_clause
--            group_clause having_clause window_clause
--                {
--                    SelectStmt *n = makeNode(SelectStmt);
--                    n->targetList = $3;
--                    n->intoClause = $4;
--                    n->fromClause = $5;
--                    n->whereClause = $6;
--                    n->groupClause = $7;
--                    n->havingClause = $8;
--                    n->windowClause = $9;
--                    $$ = (Node *)n;
--                }
--            | SELECT distinct_clause target_list
--            into_clause from_clause where_clause
--            group_clause having_clause window_clause
--                {
--                    SelectStmt *n = makeNode(SelectStmt);
--                    n->distinctClause = $2;
--                    n->targetList = $3;
--                    n->intoClause = $4;
--                    n->fromClause = $5;
--                    n->whereClause = $6;
--                    n->groupClause = $7;
--                    n->havingClause = $8;
--                    n->windowClause = $9;
--                    $$ = (Node *)n;
--                }
            : values_clause                            { SelectValues $1 }
-- TODO select * in AST
--            | TABLE relation_expr
--                {
--                    /* same as SELECT * FROM relation_expr */
--                    ColumnRef *cr = makeNode(ColumnRef);
--                    ResTarget *rt = makeNode(ResTarget);
--                    SelectStmt *n = makeNode(SelectStmt);
--
--                    cr->fields = list_make1(makeNode(A_Star));
--                    cr->location = -1;
--
--                    rt->name = NULL;
--                    rt->indirection = NIL;
--                    rt->val = (Node *)cr;
--                    rt->location = -1;
--
--                    n->targetList = list_make1(rt);
--                    n->fromClause = list_make1($2);
--                    $$ = (Node *)n;
--                }
-- TODO UNION in AST
--            | select_clause UNION all_or_distinct select_clause
--                {
--                    $$ = makeSetOp(SETOP_UNION, $3, $1, $4);
--                }
--            | select_clause INTERSECT all_or_distinct select_clause
--                {
--                    $$ = makeSetOp(SETOP_INTERSECT, $3, $1, $4);
--                }
--            | select_clause EXCEPT all_or_distinct select_clause
--                {
--                    $$ = makeSetOp(SETOP_EXCEPT, $3, $1, $4);
--                }

values_clause
    : VALUES '(' expr_list ')' { NE.fromList (reverse $3) :| [] }
    | values_clause COMMA '(' expr_list ')' { NE.cons (NE.fromList (reverse $4)) $1 }

Select :: { OldSelect }
    : SELECT expr_list FROM Name WHERE Condition { Select { table = $4, columns = NE.fromList (reverse $2), conditions = Just $6 } }
    | SELECT expr_list FROM Name { Select { table = $4, columns = NE.fromList (reverse $2), conditions = Nothing } }

Insert : INSERT INTO Name '(' name_list ')' VALUES '(' expr_list ')'
       { Insert { table = $3, columns = NE.fromList (reverse $5), values = NE.fromList (reverse $9) } }

Update :: { Update }
    : UPDATE Name SET SettingList WHERE Condition { Update { table = $2, settings = NE.fromList (reverse $4), conditions = Just $6 } }
    | UPDATE Name SET SettingList { Update { table = $2, settings = NE.fromList (reverse $4), conditions = Nothing } }

{- These lists are non-empty by construction, but not by type. List head is the right-most element. -}

list(el)
    : el { [$1] }
    | list(el) COMMA el { $3 : $1 }

name_list : list(Name) { $1 }

expr_list : list(Expr) { $1 }

SettingList : list(Setting) { $1 }

sort_clause
    : ORDER BY sortby_list { $3}

sortby_list : list(sortby) { $1 }

sortby
--: a_expr USING qual_all_Op opt_nulls_order
--				{
--					$$ = makeNode(SortBy);
--					$$->node = $1;
--					$$->sortby_dir = SORTBY_USING;
--					$$->sortby_nulls = $4;
--					$$->useOp = $3;
--					$$->location = @3;
--				}
    : a_expr opt_asc_desc opt_nulls_order { SortBy $1 $2 }

opt_asc_desc
    : ASC { Ascending }
    | DESC { Descending }
    | {- EMPTY -} { DefaultSortOrder }

opt_nulls_order
    : { Nothing }
    -- : NULLS_LA FIRST_P			{ $$ = SORTBY_NULLS_FIRST; }
	-- | NULLS_LA LAST_P				{ $$ = SORTBY_NULLS_LAST; }
	-- | {- EMPTY -} { $$ = SORTBY_NULLS_DEFAULT; }

-- TODO
a_expr : Expr { $1 }

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

happyError :: L.LocToken -> Alex a
happyError (L.LocToken p t) =
  L.alexErrorPosn p ("parse error at token '" ++ L.unLex t ++ "'")

parseQuery :: FilePath -> String -> Either String Query
parseQuery = L.runAlexWithFilepath parseQuery_

parseCondition :: FilePath -> String -> Either String Condition
parseCondition = L.runAlexWithFilepath parseCondition_

parseExpr :: FilePath -> String -> Either String Expr
parseExpr = L.runAlexWithFilepath parseExpr_

lexwrap :: (L.LocToken -> Alex a) -> Alex a
lexwrap = (L.alexMonadScan' >>=)

}
