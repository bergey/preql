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

 -- NOTES
 --	  CAPITALS are used to represent terminal symbols.
 --	  non-capitals are used to represent non-terminals.

-- This Haskell port generally follows the convention above, taken from the PostgreSQL bison source.

%left		UNION EXCEPT
%left		INTERSECT
%left OR
%left AND
%right NOT
%right '='
%left '<' '>'
%nonassoc LIKE ILIKE
%left '!=' '<=' '>='
%nonassoc NOTNULL
%nonassoc ISNULL
%nonassoc IS
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
    USING { LocToken _ L.Using }
    OPERATOR { LocToken _ L.Operator }
    NULLS { LocToken _ L.Nulls }
    FIRST { LocToken _ L.First }
    LAST { LocToken _ L.Last }
    ALL { LocToken _ L.All }
    DISTINCT { LocToken _ L.Distinct }
    ON { LocToken _ L.On }
    AS { LocToken _ L.As }

    UNION { LocToken _ L.By }
    EXCEPT { LocToken _ L.Except }

    FROM { LocToken _ L.From }
    WHERE { LocToken _ L.Where }
    INTO { LocToken _ L.Into }
    VALUES { LocToken _ L.Values }
    SET { LocToken _ L.Set }
    '(' { LocToken _ L.LParen }
    ')' { LocToken _ L.RParen }
    COMMA { LocToken _ L.Comma }

    IDENT { LocToken _ (L.Name $$) }
    STRING { LocToken _ (L.String $$) }
    NUMBER { LocToken _ (L.Number $$) }
    PARAM { LocToken _ (L.NumberedParam $$) }
    HASKELL_PARAM { LocToken _ (L.HaskellParam $$) }

    '+' { LocToken _ L.Add }
    '-' { LocToken _ L.Sub }
    '*' { LocToken _ L.Mul }
    '/' { LocToken _ L.Div }
    '%' { LocToken _ L.Mod }
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

-- A complete SELECT statement looks like this.
--
-- The rule returns either a single SelectStmt node or a tree of them,
-- representing a set-operation tree.
--
-- There is an ambiguity when a sub-SELECT is within an a_expr and there
-- are excess parentheses: do the parentheses belong to the sub-SELECT or
-- to the surrounding a_expr?  We don't really care, but bison wants to know.
-- To resolve the ambiguity, we are careful to define the grammar so that
-- the decision is staved off as long as possible: as long as we can keep
-- absorbing parentheses into the sub-SELECT, we will do so, and only when
-- it's no longer possible to do that will we decide that parens belong to
-- the expression.	For example, in "SELECT (((SELECT 2)) + 3)" the extra
-- parentheses are treated as part of the sub-select.  The necessity of doing
-- it that way is shown by "SELECT (((SELECT 2)) UNION SELECT 2)".	Had we
-- parsed "((SELECT 2))" as an a_expr, it'd be too late to go back to the
-- SELECT viewpoint when we see the UNION.
--
-- This approach is implemented by defining a nonterminal select_with_parens,
-- which represents a SELECT with at least one outer layer of parentheses,
-- and being careful to use select_with_parens, never '(' SelectStmt ')',
-- in the expression grammar.  We will then have shift-reduce conflicts
-- which we can resolve in favor of always treating '(' <select> ')' as
-- a select_with_parens.  To resolve the conflicts, the productions that
-- conflict with the select_with_parens productions are manually given
-- precedences lower than the precedence of ')', thereby ensuring that we
-- shift ')' (and then reduce to select_with_parens) rather than trying to
-- reduce the inner <select> nonterminal to something else.  We use UMINUS
-- precedence for this, which is a fairly arbitrary choice.
--
-- To be able to define select_with_parens itself without ambiguity, we need
-- a nonterminal select_no_parens that represents a SELECT structure with no
-- outermost parentheses.  This is a little bit tedious, but it works.
--
-- In non-expression contexts, we use SelectStmt which can represent a SELECT
-- with or without outer parentheses.

SelectStmt :: { SelectStmt }
    : select_no_parens { $1 }
    | select_with_parens { $1 }

select_with_parens
    : '(' select_no_parens ')' { $2 }
    | '(' select_with_parens ')' { $2 }

--  This rule parses the equivalent of the standard's <query expression>.
--  The duplicative productions are annoying, but hard to get rid of without
--  creating shift/reduce conflicts.
--
-- 	The locking clause (FOR UPDATE etc) may be before or after LIMIT/OFFSET.
-- 	In <=7.2.X, LIMIT/OFFSET had to be after FOR UPDATE
-- 	We now support both orderings, but prefer LIMIT/OFFSET before the locking
--  clause.
-- 	2002-08-28 bjm

select_no_parens :: { SelectStmt }
    : simple_select { SimpleSelect $1 }
    | select_clause sort_clause { SortedSelect $1 $2 }
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

-- This rule parses SELECT statements that can appear within set operations,
-- including UNION, INTERSECT and EXCEPT.  '(' and ')' can be used to specify
-- the ordering of the set operations.	Without '(' and ')' we want the
-- operations to be ordered per the precedence specs at the head of this file.
--
-- As with select_no_parens, simple_select cannot have outer parentheses,
-- but can have parenthesized subclauses.
--
-- Note that sort clauses cannot be included at this level --- SQL requires
--		SELECT foo UNION SELECT bar ORDER BY baz
-- to be parsed as
--		(SELECT foo UNION SELECT bar) ORDER BY baz
-- not
--		SELECT foo UNION (SELECT bar ORDER BY baz)
-- Likewise for WITH, FOR UPDATE and LIMIT.  Therefore, those clauses are
-- described as part of the select_no_parens production, not simple_select.
-- This does not limit functionality, because you can reintroduce these
-- clauses inside parentheses.
--
-- NOTE: only the leftmost component SelectStmt should have INTO.
-- However, this is not checked by the grammar; parse analysis must check it.

simple_select :: { SimpleSelect }
           : SELECT opt_all_clause opt_target_list
           into_clause from_clause where_clause
           group_clause having_clause window_clause { SelectUnordered (Unordered Nothing $3) }
-- TODO WIP
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
            | values_clause                            { SelectValues $1 }
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
    : SELECT expr_list FROM Name WHERE Condition { OldSelect { table = $4, columns = NE.fromList (reverse $2), conditions = Just $6 } }
    | SELECT expr_list FROM Name { OldSelect { table = $4, columns = NE.fromList (reverse $2), conditions = Nothing } }

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

all_or_distinct :: { AllOrDistinct }
    : ALL { All }
    | DISTINCT { Distinct }
    | { Distinct }

-- We use (DistinctAll) as a placeholder to indicate that all target expressions
-- should be placed in the DISTINCT list during parsetree analysis.
distinct_clause :: { DistinctClause }
    : DISTINCT { DistinctAll }
    | DISTINCT ON '(' expr_list ')' { DistinctOn $4 }

opt_all_clause
    : ALL { () }
    | { () }

opt_sort_clause :: { [SortBy ] }
    : sort_clause { NE.toList $1 }
    | { [] }

sort_clause :: { NonEmpty SortBy }
    : ORDER BY sortby_list { NE.fromList (reverse $3) }

sortby_list : list(sortby) { $1 }

sortby
    : a_expr USING qual_all_Op opt_nulls_order { SortBy $1 (Using $3) $4 }
    | a_expr opt_asc_desc opt_nulls_order { SortBy $1 (SortOrder $2) $3 }

opt_asc_desc
    : ASC { Ascending }
    | DESC { Descending }
    | {- EMPTY -} { DefaultSortOrder }

opt_nulls_order
    : NULLS FIRST			{ NullsFirst }
	| NULLS LAST				{ NullsLast }
	|  { NullsOrderDefault }

any_operator: all_Op { $1 }
-- We don't yet support schema-qualified operators (they're more useful if user-defined)

all_Op : MathOp { $1 }
-- We don't (yet?) support user-defined operators

MathOp :: { BinOp }
    : '+'									{ Add }
    | '-'									{ Sub }
    | '*'									{ Mul }
    | '/'									{ Div }
    | '%'									{ Mod }
    | '^'									{ Exponent }
    | '<'									{ Comp LT }
    | '>'									{ Comp GT }
    | '='									{ Comp Eq }
    | '<='							{ Comp LTE }
    | '>='						{ Comp GTE }
    | '!='							{ Comp NEq }

qual_Op
    -- We don't (yet?) support user-defined operators
    -- :	Op { $1 }
    : OPERATOR '(' any_operator ')' { $3 }

qual_all_Op
    : all_Op { $1 }
    | OPERATOR '(' any_operator ')' { $3 }

-- TODO a_expr, b_expr from bison
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

Name : IDENT { mkName $1 }

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

opt_target_list :: { [ResTarget] }
    : target_list { NE.toList $1 }
    | { [] }

target_list : list(target_el) { NE.fromList (reverse $1) }

target_el :: { ResTarget }
    : a_expr AS ColLabel { ColumnTarget (ColumnRef $1 (Just $3)) }
    | a_expr IDENT { ColumnTarget (ColumnRef $1 (Just $2)) }
    | a_expr { ColumnTarget (ColumnRef $1 Nothing) }
    | '*' { Star }

-- Name classification hierarchy.
--
-- IDENT is the lexeme returned by the lexer for identifiers that match
-- no known keyword.  In most cases, we can accept certain keywords as
-- names, not only IDENTs.	We prefer to accept as many such keywords
-- as possible to minimize the impact of "reserved words" on programmers.
-- So, we divide names into several possible classes.  The classification
-- is chosen in part to make keywords acceptable as names wherever possible.
--

-- Column identifier --- names that can be column, table, etc names.
ColId
    :		IDENT									{ $1 }
    | unreserved_keyword					{ $1 }

-- Type/function identifier --- names that can be type or function names.
type_function_name
    :	IDENT							{ $1 }
    | unreserved_keyword					{ $1 }
    | type_func_name_keyword				{ $1 }

-- Any not-fully-reserved word --- these names can be, eg, role names.
NonReservedWord
     :	IDENT							{ $1 }
			| unreserved_keyword					{ $1 }
			| col_name_keyword						{ $1 }
			| type_func_name_keyword				{ $1 }

-- Column label --- allowed labels in "AS" clauses.
-- This presently includes *all* Postgres keywords.
ColLabel:	IDENT									{  $1 }
			| unreserved_keyword					{  $1 }
			| col_name_keyword						{  $1 }
			| type_func_name_keyword				{  $1 }
			| reserved_keyword						{  $1 }

-- Keyword category lists.  Generally, every keyword present in
-- the Postgres grammar should appear in exactly one of these lists.
--
-- Put a new keyword into the first list that it can go into without causing
-- shift or reduce conflicts.  The earlier lists define "less reserved"
-- categories of keywords.
--
-- Make sure that each keyword's category in kwlist.h matches where
-- it is listed here.  (Someday we may be able to generate these lists and
-- kwlist.h's table from a common master list.)

-- "Unreserved" keywords --- available for use as any kind of name.
unreserved_keyword
            : ABORT_P
			| ABSOLUTE_P
			| ACCESS
			| ACTION
			| ADD_P
			| ADMIN
			| AFTER
			| AGGREGATE
			| ALSO
			| ALTER
			| ALWAYS
			| ASSERTION
			| ASSIGNMENT
			| AT
			| ATTACH
			| ATTRIBUTE
			| BACKWARD
			| BEFORE
			| BEGIN_P
			| BY
			| CACHE
			| CALL
			| CALLED
			| CASCADE
			| CASCADED
			| CATALOG_P
			| CHAIN
			| CHARACTERISTICS
			| CHECKPOINT
			| CLASS
			| CLOSE
			| CLUSTER
			| COLUMNS
			| COMMENT
			| COMMENTS
			| COMMIT
			| COMMITTED
			| CONFIGURATION
			| CONFLICT
			| CONNECTION
			| CONSTRAINTS
			| CONTENT_P
			| CONTINUE_P
			| CONVERSION_P
			| COPY
			| COST
			| CSV
			| CUBE
			| CURRENT_P
			| CURSOR
			| CYCLE
			| DATA_P
			| DATABASE
			| DAY_P
			| DEALLOCATE
			| DECLARE
			| DEFAULTS
			| DEFERRED
			| DEFINER
			| DELETE_P
			| DELIMITER
			| DELIMITERS
			| DEPENDS
			| DETACH
			| DICTIONARY
			| DISABLE_P
			| DISCARD
			| DOCUMENT_P
			| DOMAIN_P
			| DOUBLE_P
			| DROP
			| EACH
			| ENABLE_P
			| ENCODING
			| ENCRYPTED
			| ENUM_P
			| ESCAPE
			| EVENT
			| EXCLUDE
			| EXCLUDING
			| EXCLUSIVE
			| EXECUTE
			| EXPLAIN
			| EXTENSION
			| EXTERNAL
			| FAMILY
			| FILTER
			| FIRST_P
			| FOLLOWING
			| FORCE
			| FORWARD
			| FUNCTION
			| FUNCTIONS
			| GENERATED
			| GLOBAL
			| GRANTED
			| GROUPS
			| HANDLER
			| HEADER_P
			| HOLD
			| HOUR_P
			| IDENTITY_P
			| IF_P
			| IMMEDIATE
			| IMMUTABLE
			| IMPLICIT_P
			| IMPORT_P
			| INCLUDE
			| INCLUDING
			| INCREMENT
			| INDEX
			| INDEXES
			| INHERIT
			| INHERITS
			| INLINE_P
			| INPUT_P
			| INSENSITIVE
			| INSERT
			| INSTEAD
			| INVOKER
			| ISOLATION
			| KEY
			| LABEL
			| LANGUAGE
			| LARGE_P
			| LAST_P
			| LEAKPROOF
			| LEVEL
			| LISTEN
			| LOAD
			| LOCAL
			| LOCATION
			| LOCK_P
			| LOCKED
			| LOGGED
			| MAPPING
			| MATCH
			| MATERIALIZED
			| MAXVALUE
			| METHOD
			| MINUTE_P
			| MINVALUE
			| MODE
			| MONTH_P
			| MOVE
			| NAME_P
			| NAMES
			| NEW
			| NEXT
			| NO
			| NOTHING
			| NOTIFY
			| NOWAIT
			| NULLS_P
			| OBJECT_P
			| OF
			| OFF
			| OIDS
			| OLD
			| OPERATOR
			| OPTION
			| OPTIONS
			| ORDINALITY
			| OTHERS
			| OVER
			| OVERRIDING
			| OWNED
			| OWNER
			| PARALLEL
			| PARSER
			| PARTIAL
			| PARTITION
			| PASSING
			| PASSWORD
			| PLANS
			| POLICY
			| PRECEDING
			| PREPARE
			| PREPARED
			| PRESERVE
			| PRIOR
			| PRIVILEGES
			| PROCEDURAL
			| PROCEDURE
			| PROCEDURES
			| PROGRAM
			| PUBLICATION
			| QUOTE
			| RANGE
			| READ
			| REASSIGN
			| RECHECK
			| RECURSIVE
			| REF
			| REFERENCING
			| REFRESH
			| REINDEX
			| RELATIVE_P
			| RELEASE
			| RENAME
			| REPEATABLE
			| REPLACE
			| REPLICA
			| RESET
			| RESTART
			| RESTRICT
			| RETURNS
			| REVOKE
			| ROLE
			| ROLLBACK
			| ROLLUP
			| ROUTINE
			| ROUTINES
			| ROWS
			| RULE
			| SAVEPOINT
			| SCHEMA
			| SCHEMAS
			| SCROLL
			| SEARCH
			| SECOND_P
			| SECURITY
			| SEQUENCE
			| SEQUENCES
			| SERIALIZABLE
			| SERVER
			| SESSION
			| SET
			| SETS
			| SHARE
			| SHOW
			| SIMPLE
			| SKIP
			| SNAPSHOT
			| SQL_P
			| STABLE
			| STANDALONE_P
			| START
			| STATEMENT
			| STATISTICS
			| STDIN
			| STDOUT
			| STORAGE
			| STORED
			| STRICT_P
			| STRIP_P
			| SUBSCRIPTION
			| SUPPORT
			| SYSID
			| SYSTEM_P
			| TABLES
			| TABLESPACE
			| TEMP
			| TEMPLATE
			| TEMPORARY
			| TEXT_P
			| TIES
			| TRANSACTION
			| TRANSFORM
			| TRIGGER
			| TRUNCATE
			| TRUSTED
			| TYPE_P
			| TYPES_P
			| UNBOUNDED
			| UNCOMMITTED
			| UNENCRYPTED
			| UNKNOWN
			| UNLISTEN
			| UNLOGGED
			| UNTIL
			| UPDATE
			| VACUUM
			| VALID
			| VALIDATE
			| VALIDATOR
			| VALUE_P
			| VARYING
			| VERSION_P
			| VIEW
			| VIEWS
			| VOLATILE
			| WHITESPACE_P
			| WITHIN
			| WITHOUT
			| WORK
			| WRAPPER
			| WRITE
			| XML_P
			| YEAR_P
			| YES_P
			| ZONE

-- Column identifier --- keywords that can be column, table, etc names.
--
-- Many of these keywords will in fact be recognized as type or function
-- names too; but they have special productions for the purpose, and so
-- can't be treated as "generic" type or function names.
--
-- The type names appearing here are not usable as function names
-- because they can be followed by '(' in typename productions, which
-- looks too much like a function call for an LR(1) parser.
col_name_keyword:
			  BETWEEN
			| BIGINT
			| BIT
			| BOOLEAN_P
			| CHAR_P
			| CHARACTER
			| COALESCE
			| DEC
			| DECIMAL_P
			| EXISTS
			| EXTRACT
			| FLOAT_P
			| GREATEST
			| GROUPING
			| INOUT
			| INT_P
			| INTEGER
			| INTERVAL
			| LEAST
			| NATIONAL
			| NCHAR
			| NONE
			| NULLIF
			| NUMERIC
			| OUT_P
			| OVERLAY
			| POSITION
			| PRECISION
			| REAL
			| ROW
			| SETOF
			| SMALLINT
			| SUBSTRING
			| TIME
			| TIMESTAMP
			| TREAT
			| TRIM
			| VALUES
			| VARCHAR
			| XMLATTRIBUTES
			| XMLCONCAT
			| XMLELEMENT
			| XMLEXISTS
			| XMLFOREST
			| XMLNAMESPACES
			| XMLPARSE
			| XMLPI
			| XMLROOT
			| XMLSERIALIZE
			| XMLTABLE

-- Type/function identifier --- keywords that can be type or function names.
--
-- Most of these are keywords that are used as operators in expressions;
-- in general such keywords can't be column names because they would be
-- ambiguous with variables, but they are unambiguous as function identifiers.
--
-- Do not include POSITION, SUBSTRING, etc here since they have explicit
-- productions in a_expr to support the goofy SQL9x argument syntax.
-- - thomas 2000-11-28
type_func_name_keyword:
			  AUTHORIZATION
			| BINARY
			| COLLATION
			| CONCURRENTLY
			| CROSS
			| CURRENT_SCHEMA
			| FREEZE
			| FULL
			| ILIKE
			| INNER_P
			| IS
			| ISNULL
			| JOIN
			| LEFT
			| LIKE
			| NATURAL
			| NOTNULL
			| OUTER_P
			| OVERLAPS
			| RIGHT
			| SIMILAR
			| TABLESAMPLE
			| VERBOSE

-- Reserved keyword --- these keywords are usable only as a ColLabel.
--
-- Keywords appear here if they could not be distinguished from variable,
-- type, or function names in some contexts.  Don't put things here unless
-- forced to.
reserved_keyword:
			  ALL
			| ANALYSE
			| ANALYZE
			| AND
			| ANY
			| ARRAY
			| AS
			| ASC
			| ASYMMETRIC
			| BOTH
			| CASE
			| CAST
			| CHECK
			| COLLATE
			| COLUMN
			| CONSTRAINT
			| CREATE
			| CURRENT_CATALOG
			| CURRENT_DATE
			| CURRENT_ROLE
			| CURRENT_TIME
			| CURRENT_TIMESTAMP
			| CURRENT_USER
			| DEFAULT
			| DEFERRABLE
			| DESC
			| DISTINCT
			| DO
			| ELSE
			| END_P
			| EXCEPT
			| FALSE_P
			| FETCH
			| FOR
			| FOREIGN
			| FROM
			| GRANT
			| GROUP_P
			| HAVING
			| IN_P
			| INITIALLY
			| INTERSECT
			| INTO
			| LATERAL_P
			| LEADING
			| LIMIT
			| LOCALTIME
			| LOCALTIMESTAMP
			| NOT
			| NULL_P
			| OFFSET
			| ON
			| ONLY
			| OR
			| ORDER
			| PLACING
			| PRIMARY
			| REFERENCES
			| RETURNING
			| SELECT
			| SESSION_USER
			| SOME
			| SYMMETRIC
			| TABLE
			| THEN
			| TO
			| TRAILING
			| TRUE_P
			| UNION
			| UNIQUE
			| USER
			| USING
			| VARIADIC
			| WHEN
			| WHERE
			| WINDOW
			| WITH

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
