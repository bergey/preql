{
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Preql.QuasiQuoter.Syntax.Parser (parseStatement, parseSelect, parseExpr) where

import Preql.QuasiQuoter.Syntax.Syntax
import Preql.QuasiQuoter.Syntax.Name
import Preql.QuasiQuoter.Syntax.Lex (Alex, LocToken(..), Token)

import Data.Text (Text)
import           Prelude hiding (LT, GT, lex)
import           Control.Monad (when)
import           Data.List.NonEmpty        (NonEmpty (..))

import qualified Preql.QuasiQuoter.Syntax.Lex as L
import qualified Data.List.NonEmpty as NE
}

%name parseStatement_ Statement
%name parseSelect_ SelectStmt
%name parseExpr_ a_expr
%tokentype { L.LocToken }
%monad { Alex }
%lexer { lexwrap } {  L.LocToken _ L.EOF }
%error { happyError }

 -- * NOTES
 -- *	  CAPITALS are used to represent terminal symbols.
 -- *	  non-capitals are used to represent non-terminals.

-- This Haskell port generally follows the convention above, taken from the PostgreSQL bison source.
-- Comments with a leading * are taken from the PostgreSQL source.
-- Unimplemnted parts of the official parser are marked TODO, and generally contain bison & C syntax.

-- * Precedence: lowest to highest
%nonassoc	SET -- * see relation_expr_opt_alias
%left		UNION EXCEPT
%left		INTERSECT
%left OR
%left AND
%right NOT
%nonassoc	IS ISNULL NOTNULL -- * IS sets precedence for IS NULL, etc
%nonassoc '<' '>' '=' '!=' '<=' '>='
%nonassoc	BETWEEN IN_P LIKE ILIKE SIMILAR NOT_LA
%nonassoc	ESCAPE			-- * ESCAPE must be just above LIKE/ILIKE/SIMILAR
%left		POSTFIXOP		-- * dummy for postfix Op rules
-- * To support target_el without AS, we must give IDENT an explicit priority
-- * between POSTFIXOP and Op.  We can safely assign the same priority to
-- * various unreserved keywords as needed to resolve ambiguities (this can't
-- * have any bad effects since obviously the keywords will still behave the
-- * same as if they weren't keywords).  We need to do this:
-- * for PARTITION, RANGE, ROWS, GROUPS to support opt_existing_window_name;
-- * for RANGE, ROWS, GROUPS so that they can follow a_expr without creating
-- * postfix-operator problems;
-- * for GENERATED so that it can follow b_expr;
-- * and for NULL so that it can follow b_expr in ColQualList without creating
-- * postfix-operator problems.
-- *
-- * To support CUBE and ROLLUP in GROUP BY without reserving them, we give them
-- * an explicit priority lower than '(', so that a rule with CUBE '(' will shift
-- * rather than reducing a conflicting rule that takes CUBE as a function name.
-- * Using the same precedence as IDENT seems right for the reasons given above.
-- *
-- * The frame_bound productions UNBOUNDED PRECEDING and UNBOUNDED FOLLOWING
-- * are even messier: since UNBOUNDED is an unreserved keyword (per spec!),
-- * there is no principled way to distinguish these from the productions
-- * a_expr PRECEDING/FOLLOWING.  We hack this up by giving UNBOUNDED slightly
-- * lower precedence than PRECEDING and FOLLOWING.  At present this doesn't
-- * appear to cause UNBOUNDED to be treated differently from other unreserved
-- * keywords anywhere else in the grammar, but it's definitely risky.  We can
-- * blame any funny behavior of UNBOUNDED on the SQL standard, though.
%nonassoc	UNBOUNDED		-- * ideally should have same precedence as IDENT
%nonassoc	IDENT GENERATED NULL_P PARTITION RANGE ROWS GROUPS PRECEDING FOLLOWING CUBE ROLLUP
%left		Op OPERATOR		-- * multi-character ops and user-defined operators
%left '+' '-'
%left '*' '/' '%'
%left '^'
-- * Unary Operators
%left		AT  -- * sets precedence for AT TIME ZONE
%left		COLLATE
%right		UMINUS
%left		'[' ']'
%left		'(' ')'
%left		TYPECAST
%left		'.'
-- * These might seem to be low-precedence, but actually they are not part
-- * of the arithmetic hierarchy at all in their use as JOIN operators.
-- * We make them high-precedence to support their use as function names.
-- * They wouldn't be given a precedence at all, were it not that we need
-- * left-associativity among the JOIN rules themselves.
%left JOIN CROSS LEFT FULL RIGHT INNER_P NATURAL
-- * kluge to keep xml_whitespace_option from causing shift/reduce conflicts
%right		PRESERVE STRIP_P


%token
    DELETE { LocToken _ L.DELETE_P }
    SELECT { LocToken _ L.SELECT }
    INSERT { LocToken _ L.INSERT }
    UPDATE { LocToken _ L.UPDATE }

    ASC { LocToken _ L.ASC }
    DESC { LocToken _ L.DESC }
    ORDER { LocToken _ L.ORDER }
    BY { LocToken _ L.BY }
    USING { LocToken _ L.USING }
    OPERATOR { LocToken _ L.OPERATOR }
    NULLS { LocToken _ L.Nulls }
    FIRST { LocToken _ L.First }
    ALL { LocToken _ L.ALL }
    DISTINCT { LocToken _ L.DISTINCT }
    ON { LocToken _ L.ON }
    AS { LocToken _ L.AS }

    UNION { LocToken _ L.UNION }
    EXCEPT { LocToken _ L.EXCEPT }

    FROM { LocToken _ L.FROM }
    WHERE { LocToken _ L.WHERE }
    INTO { LocToken _ L.INTO }
    VALUES { LocToken _ L.VALUES }
    SET { LocToken _ L.SET }
    '(' { LocToken _ L.LParen }
    ',' { LocToken _ L.Comma }
    ')' { LocToken _ L.RParen }
    '.' { LocToken _ L.Dot }

    IDENT { LocToken _ (L.Name $$) }
    STRING { LocToken _ (L.String $$) }
    Iconst { LocToken _ (L.Iconst $$) }
    Fconst { LocToken _ (L.Fconst $$) }
    PARAM { LocToken _ (L.NumberedParam $$) }
    HASKELL_PARAM { LocToken _ (L.HaskellParam $$) }

    '+' { LocToken _ L.Add }
    '-' { LocToken _ L.Sub }
    '*' { LocToken _ L.Mul }
    '/' { LocToken _ L.Div }
    '%' { LocToken _ L.Mod }
    '^' { LocToken _ L.Exponent }

    IS { LocToken _ L.IS }
    ISNULL { LocToken _ L.ISNULL }
    NOTNULL { LocToken _ L.NOTNULL }

    '=' { LocToken _ L.Equals }
    '!=' { LocToken _ L.NotEquals }
    '<' { L.LocToken _ L.LT }
    '>' { L.LocToken _ L.GT }
    '<=' { L.LocToken _ L.LTE }
    '>=' { L.LocToken _ L.GTE }
    NOT { L.LocToken _ L.NOT }
    LIKE { L.LocToken _ L.LIKE }
    ILIKE { L.LocToken _ L.ILIKE }

    AND  { LocToken _ L.AND }
    OR { LocToken _ L.OR }

    SEMICOLON { LocToken _ L.Semicolon }

-- Non-keyword token types.  These are separate in the bison parser
-- for technical reasons, and here to make coordination with the bison
-- parser easier.
    COLON_EQUALS { L.LocToken _ L.COLON_EQUALS }
    EQUALS_GREATER { L.LocToken _ L.EQUALS_GREATER }

    -- all the keywords not mentioned above, from bison
    ABORT_P { L.LocToken _ L.ABORT_P }
    AUTHORIZATION { L.LocToken _ L.AUTHORIZATION }
    BETWEEN { L.LocToken _ L.BETWEEN }
    ABSOLUTE_P { L.LocToken _ L.ABSOLUTE_P }
    ACCESS { L.LocToken _ L.ACCESS }
    ACTION { L.LocToken _ L.ACTION }
    ADD_P { L.LocToken _ L.ADD_P }
    ADMIN { L.LocToken _ L.ADMIN }
    AFTER { L.LocToken _ L.AFTER }
    AGGREGATE { L.LocToken _ L.AGGREGATE }
    ALSO { L.LocToken _ L.ALSO }
    ALTER { L.LocToken _ L.ALTER }
    ALWAYS { L.LocToken _ L.ALWAYS }
    ANALYSE { L.LocToken _ L.ANALYSE }
    ANALYZE { L.LocToken _ L.ANALYZE }
    ANY { L.LocToken _ L.ANY }
    ARRAY { L.LocToken _ L.ARRAY }
    ASSERTION { L.LocToken _ L.ASSERTION }
    ASSIGNMENT { L.LocToken _ L.ASSIGNMENT }
    ASYMMETRIC { L.LocToken _ L.ASYMMETRIC }
    AT { L.LocToken _ L.AT }
    ATTACH { L.LocToken _ L.ATTACH }
    ATTRIBUTE { L.LocToken _ L.ATTRIBUTE }
    BACKWARD { L.LocToken _ L.BACKWARD }
    BEFORE { L.LocToken _ L.BEFORE }
    BEGIN_P { L.LocToken _ L.BEGIN_P }
    BIGINT { L.LocToken _ L.BIGINT }
    BINARY { L.LocToken _ L.BINARY }
    BIT { L.LocToken _ L.BIT }
    BOOLEAN_P { L.LocToken _ L.BOOLEAN_P }
    BOTH { L.LocToken _ L.BOTH }
    CACHE { L.LocToken _ L.CACHE }
    CALL { L.LocToken _ L.CALL }
    CALLED { L.LocToken _ L.CALLED }
    CASCADE { L.LocToken _ L.CASCADE }
    CASCADED { L.LocToken _ L.CASCADED }
    CASE { L.LocToken _ L.CASE }
    CAST { L.LocToken _ L.CAST }
    CATALOG_P { L.LocToken _ L.CATALOG_P }
    CHAIN { L.LocToken _ L.CHAIN }
    CHARACTER { L.LocToken _ L.CHARACTER }
    CHARACTERISTICS { L.LocToken _ L.CHARACTERISTICS }
    CHAR_P { L.LocToken _ L.CHAR_P }
    CHECK { L.LocToken _ L.CHECK }
    CHECKPOINT { L.LocToken _ L.CHECKPOINT }
    CLASS { L.LocToken _ L.CLASS }
    CLOSE { L.LocToken _ L.CLOSE }
    CLUSTER { L.LocToken _ L.CLUSTER }
    COALESCE { L.LocToken _ L.COALESCE }
    COLLATE { L.LocToken _ L.COLLATE }
    COLLATION { L.LocToken _ L.COLLATION }
    COLUMN { L.LocToken _ L.COLUMN }
    COLUMNS { L.LocToken _ L.COLUMNS }
    COMMENT { L.LocToken _ L.COMMENT }
    COMMENTS { L.LocToken _ L.COMMENTS }
    COMMIT { L.LocToken _ L.COMMIT }
    COMMITTED { L.LocToken _ L.COMMITTED }
    CONCURRENTLY { L.LocToken _ L.CONCURRENTLY }
    CONFIGURATION { L.LocToken _ L.CONFIGURATION }
    CONFLICT { L.LocToken _ L.CONFLICT }
    CONNECTION { L.LocToken _ L.CONNECTION }
    CONSTRAINT { L.LocToken _ L.CONSTRAINT }
    CONSTRAINTS { L.LocToken _ L.CONSTRAINTS }
    CONTENT_P { L.LocToken _ L.CONTENT_P }
    CONTINUE_P { L.LocToken _ L.CONTINUE_P }
    CONVERSION_P { L.LocToken _ L.CONVERSION_P }
    COPY { L.LocToken _ L.COPY }
    COST { L.LocToken _ L.COST }
    CREATE { L.LocToken _ L.CREATE }
    CROSS { L.LocToken _ L.CROSS }
    CSV { L.LocToken _ L.CSV }
    CUBE { L.LocToken _ L.CUBE }
    CURRENT_CATALOG { L.LocToken _ L.CURRENT_CATALOG }
    CURRENT_DATE { L.LocToken _ L.CURRENT_DATE }
    CURRENT_P { L.LocToken _ L.CURRENT_P }
    CURRENT_ROLE { L.LocToken _ L.CURRENT_ROLE }
    CURRENT_SCHEMA { L.LocToken _ L.CURRENT_SCHEMA }
    CURRENT_TIME { L.LocToken _ L.CURRENT_TIME }
    CURRENT_TIMESTAMP { L.LocToken _ L.CURRENT_TIMESTAMP }
    CURRENT_USER { L.LocToken _ L.CURRENT_USER }
    CURSOR { L.LocToken _ L.CURSOR }
    CYCLE { L.LocToken _ L.CYCLE }
    DATABASE { L.LocToken _ L.DATABASE }
    DATA_P { L.LocToken _ L.DATA_P }
    DAY_P { L.LocToken _ L.DAY_P }
    DEALLOCATE { L.LocToken _ L.DEALLOCATE }
    DEC { L.LocToken _ L.DEC }
    DECIMAL_P { L.LocToken _ L.DECIMAL_P }
    DECLARE { L.LocToken _ L.DECLARE }
    DEFAULT { L.LocToken _ L.DEFAULT }
    DEFAULTS { L.LocToken _ L.DEFAULTS }
    DEFERRABLE { L.LocToken _ L.DEFERRABLE }
    DEFERRED { L.LocToken _ L.DEFERRED }
    DEFINER { L.LocToken _ L.DEFINER }
    DELETE_P { L.LocToken _ L.DELETE_P }
    DELIMITER { L.LocToken _ L.DELIMITER }
    DELIMITERS { L.LocToken _ L.DELIMITERS }
    DEPENDS { L.LocToken _ L.DEPENDS }
    DETACH { L.LocToken _ L.DETACH }
    DICTIONARY { L.LocToken _ L.DICTIONARY }
    DISABLE_P { L.LocToken _ L.DISABLE_P }
    DISCARD { L.LocToken _ L.DISCARD }
    DO { L.LocToken _ L.DO }
    DOCUMENT_P { L.LocToken _ L.DOCUMENT_P }
    DOMAIN_P { L.LocToken _ L.DOMAIN_P }
    DOUBLE_P { L.LocToken _ L.DOUBLE_P }
    DROP { L.LocToken _ L.DROP }
    EACH { L.LocToken _ L.EACH }
    ELSE { L.LocToken _ L.ELSE }
    ENABLE_P { L.LocToken _ L.ENABLE_P }
    ENCODING { L.LocToken _ L.ENCODING }
    ENCRYPTED { L.LocToken _ L.ENCRYPTED }
    END_P { L.LocToken _ L.END_P }
    ENUM_P { L.LocToken _ L.ENUM_P }
    ESCAPE { L.LocToken _ L.ESCAPE }
    EVENT { L.LocToken _ L.EVENT }
    EXCLUDE { L.LocToken _ L.EXCLUDE }
    EXCLUDING { L.LocToken _ L.EXCLUDING }
    EXCLUSIVE { L.LocToken _ L.EXCLUSIVE }
    EXECUTE { L.LocToken _ L.EXECUTE }
    EXISTS { L.LocToken _ L.EXISTS }
    EXPLAIN { L.LocToken _ L.EXPLAIN }
    EXTENSION { L.LocToken _ L.EXTENSION }
    EXTERNAL { L.LocToken _ L.EXTERNAL }
    EXTRACT { L.LocToken _ L.EXTRACT }
    FALSE_P { L.LocToken _ L.FALSE_P }
    FAMILY { L.LocToken _ L.FAMILY }
    FETCH { L.LocToken _ L.FETCH }
    FILTER { L.LocToken _ L.FILTER }
    FIRST_P { L.LocToken _ L.FIRST_P }
    FLOAT_P { L.LocToken _ L.FLOAT_P }
    FOLLOWING { L.LocToken _ L.FOLLOWING }
    FOR { L.LocToken _ L.FOR }
    FORCE { L.LocToken _ L.FORCE }
    FOREIGN { L.LocToken _ L.FOREIGN }
    FORWARD { L.LocToken _ L.FORWARD }
    FREEZE { L.LocToken _ L.FREEZE }
    FULL { L.LocToken _ L.FULL }
    FUNCTION { L.LocToken _ L.FUNCTION }
    FUNCTIONS { L.LocToken _ L.FUNCTIONS }
    GENERATED { L.LocToken _ L.GENERATED }
    GLOBAL { L.LocToken _ L.GLOBAL }
    GRANT { L.LocToken _ L.GRANT }
    GRANTED { L.LocToken _ L.GRANTED }
    GREATEST { L.LocToken _ L.GREATEST }
    GROUPING { L.LocToken _ L.GROUPING }
    GROUPS { L.LocToken _ L.GROUPS }
    GROUP_P { L.LocToken _ L.GROUP_P }
    HANDLER { L.LocToken _ L.HANDLER }
    HAVING { L.LocToken _ L.HAVING }
    HEADER_P { L.LocToken _ L.HEADER_P }
    HOLD { L.LocToken _ L.HOLD }
    HOUR_P { L.LocToken _ L.HOUR_P }
    IDENTITY_P { L.LocToken _ L.IDENTITY_P }
    IF_P { L.LocToken _ L.IF_P }
    IMMEDIATE { L.LocToken _ L.IMMEDIATE }
    IMMUTABLE { L.LocToken _ L.IMMUTABLE }
    IMPLICIT_P { L.LocToken _ L.IMPLICIT_P }
    IMPORT_P { L.LocToken _ L.IMPORT_P }
    INCLUDE { L.LocToken _ L.INCLUDE }
    INCLUDING { L.LocToken _ L.INCLUDING }
    INCREMENT { L.LocToken _ L.INCREMENT }
    INDEX { L.LocToken _ L.INDEX }
    INDEXES { L.LocToken _ L.INDEXES }
    INHERIT { L.LocToken _ L.INHERIT }
    INHERITS { L.LocToken _ L.INHERITS }
    INITIALLY { L.LocToken _ L.INITIALLY }
    INLINE_P { L.LocToken _ L.INLINE_P }
    INNER_P { L.LocToken _ L.INNER_P }
    INOUT { L.LocToken _ L.INOUT }
    INPUT_P { L.LocToken _ L.INPUT_P }
    INSENSITIVE { L.LocToken _ L.INSENSITIVE }
    INSTEAD { L.LocToken _ L.INSTEAD }
    INTEGER { L.LocToken _ L.INTEGER }
    INTERSECT { L.LocToken _ L.INTERSECT }
    INTERVAL { L.LocToken _ L.INTERVAL }
    INT_P { L.LocToken _ L.INT_P }
    INVOKER { L.LocToken _ L.INVOKER }
    IN_P { L.LocToken _ L.IN_P }
    ISOLATION { L.LocToken _ L.ISOLATION }
    JOIN { L.LocToken _ L.JOIN }
    KEY { L.LocToken _ L.KEY }
    LABEL { L.LocToken _ L.LABEL }
    LANGUAGE { L.LocToken _ L.LANGUAGE }
    LARGE_P { L.LocToken _ L.LARGE_P }
    LAST { L.LocToken _ L.LAST }
    LATERAL_P { L.LocToken _ L.LATERAL_P }
    LEADING { L.LocToken _ L.LEADING }
    LEAKPROOF { L.LocToken _ L.LEAKPROOF }
    LEAST { L.LocToken _ L.LEAST }
    LEFT { L.LocToken _ L.LEFT }
    LEVEL { L.LocToken _ L.LEVEL }
    LIMIT { L.LocToken _ L.LIMIT }
    LISTEN { L.LocToken _ L.LISTEN }
    LOAD { L.LocToken _ L.LOAD }
    LOCAL { L.LocToken _ L.LOCAL }
    LOCALTIME { L.LocToken _ L.LOCALTIME }
    LOCALTIMESTAMP { L.LocToken _ L.LOCALTIMESTAMP }
    LOCATION { L.LocToken _ L.LOCATION }
    LOCKED { L.LocToken _ L.LOCKED }
    LOCK_P { L.LocToken _ L.LOCK_P }
    LOGGED { L.LocToken _ L.LOGGED }
    MAPPING { L.LocToken _ L.MAPPING }
    MATCH { L.LocToken _ L.MATCH }
    MATERIALIZED { L.LocToken _ L.MATERIALIZED }
    MAXVALUE { L.LocToken _ L.MAXVALUE }
    METHOD { L.LocToken _ L.METHOD }
    MINUTE_P { L.LocToken _ L.MINUTE_P }
    MINVALUE { L.LocToken _ L.MINVALUE }
    MODE { L.LocToken _ L.MODE }
    MONTH_P { L.LocToken _ L.MONTH_P }
    MOVE { L.LocToken _ L.MOVE }
    NAMES { L.LocToken _ L.NAMES }
    NAME_P { L.LocToken _ L.NAME_P }
    NATIONAL { L.LocToken _ L.NATIONAL }
    NATURAL { L.LocToken _ L.NATURAL }
    NCHAR { L.LocToken _ L.NCHAR }
    NEW { L.LocToken _ L.NEW }
    NEXT { L.LocToken _ L.NEXT }
    NO { L.LocToken _ L.NO }
    NONE { L.LocToken _ L.NONE }
    NOTHING { L.LocToken _ L.NOTHING }
    NOTIFY { L.LocToken _ L.NOTIFY }
    NOWAIT { L.LocToken _ L.NOWAIT }
    NULLIF { L.LocToken _ L.NULLIF }
    NULLS_P { L.LocToken _ L.NULLS_P }
    NULL_P { L.LocToken _ L.NULL_P }
    NUMERIC { L.LocToken _ L.NUMERIC }
    OBJECT_P { L.LocToken _ L.OBJECT_P }
    OF { L.LocToken _ L.OF }
    OFF { L.LocToken _ L.OFF }
    OFFSET { L.LocToken _ L.OFFSET }
    OIDS { L.LocToken _ L.OIDS }
    OLD { L.LocToken _ L.OLD }
    ONLY { L.LocToken _ L.ONLY }
    OPTION { L.LocToken _ L.OPTION }
    OPTIONS { L.LocToken _ L.OPTIONS }
    ORDINALITY { L.LocToken _ L.ORDINALITY }
    OTHERS { L.LocToken _ L.OTHERS }
    OUTER_P { L.LocToken _ L.OUTER_P }
    OUT_P { L.LocToken _ L.OUT_P }
    OVER { L.LocToken _ L.OVER }
    OVERLAPS { L.LocToken _ L.OVERLAPS }
    OVERLAY { L.LocToken _ L.OVERLAY }
    OVERRIDING { L.LocToken _ L.OVERRIDING }
    OWNED { L.LocToken _ L.OWNED }
    OWNER { L.LocToken _ L.OWNER }
    PARALLEL { L.LocToken _ L.PARALLEL }
    PARSER { L.LocToken _ L.PARSER }
    PARTIAL { L.LocToken _ L.PARTIAL }
    PARTITION { L.LocToken _ L.PARTITION }
    PASSING { L.LocToken _ L.PASSING }
    PASSWORD { L.LocToken _ L.PASSWORD }
    PLACING { L.LocToken _ L.PLACING }
    PLANS { L.LocToken _ L.PLANS }
    POLICY { L.LocToken _ L.POLICY }
    POSITION { L.LocToken _ L.POSITION }
    PRECEDING { L.LocToken _ L.PRECEDING }
    PRECISION { L.LocToken _ L.PRECISION }
    PREPARE { L.LocToken _ L.PREPARE }
    PREPARED { L.LocToken _ L.PREPARED }
    PRESERVE { L.LocToken _ L.PRESERVE }
    PRIMARY { L.LocToken _ L.PRIMARY }
    PRIOR { L.LocToken _ L.PRIOR }
    PRIVILEGES { L.LocToken _ L.PRIVILEGES }
    PROCEDURAL { L.LocToken _ L.PROCEDURAL }
    PROCEDURE { L.LocToken _ L.PROCEDURE }
    PROCEDURES { L.LocToken _ L.PROCEDURES }
    PROGRAM { L.LocToken _ L.PROGRAM }
    PUBLICATION { L.LocToken _ L.PUBLICATION }
    QUOTE { L.LocToken _ L.QUOTE }
    RANGE { L.LocToken _ L.RANGE }
    READ { L.LocToken _ L.READ }
    REAL { L.LocToken _ L.REAL }
    REASSIGN { L.LocToken _ L.REASSIGN }
    RECHECK { L.LocToken _ L.RECHECK }
    RECURSIVE { L.LocToken _ L.RECURSIVE }
    REF { L.LocToken _ L.REF }
    REFERENCES { L.LocToken _ L.REFERENCES }
    REFERENCING { L.LocToken _ L.REFERENCING }
    REFRESH { L.LocToken _ L.REFRESH }
    REINDEX { L.LocToken _ L.REINDEX }
    RELATIVE_P { L.LocToken _ L.RELATIVE_P }
    RELEASE { L.LocToken _ L.RELEASE }
    RENAME { L.LocToken _ L.RENAME }
    REPEATABLE { L.LocToken _ L.REPEATABLE }
    REPLACE { L.LocToken _ L.REPLACE }
    REPLICA { L.LocToken _ L.REPLICA }
    RESET { L.LocToken _ L.RESET }
    RESTART { L.LocToken _ L.RESTART }
    RESTRICT { L.LocToken _ L.RESTRICT }
    RETURNING { L.LocToken _ L.RETURNING }
    RETURNS { L.LocToken _ L.RETURNS }
    REVOKE { L.LocToken _ L.REVOKE }
    RIGHT { L.LocToken _ L.RIGHT }
    ROLE { L.LocToken _ L.ROLE }
    ROLLBACK { L.LocToken _ L.ROLLBACK }
    ROLLUP { L.LocToken _ L.ROLLUP }
    ROUTINE { L.LocToken _ L.ROUTINE }
    ROUTINES { L.LocToken _ L.ROUTINES }
    ROW { L.LocToken _ L.ROW }
    ROWS { L.LocToken _ L.ROWS }
    RULE { L.LocToken _ L.RULE }
    SAVEPOINT { L.LocToken _ L.SAVEPOINT }
    SCHEMA { L.LocToken _ L.SCHEMA }
    SCHEMAS { L.LocToken _ L.SCHEMAS }
    SCROLL { L.LocToken _ L.SCROLL }
    SEARCH { L.LocToken _ L.SEARCH }
    SECOND_P { L.LocToken _ L.SECOND_P }
    SECURITY { L.LocToken _ L.SECURITY }
    SEQUENCE { L.LocToken _ L.SEQUENCE }
    SEQUENCES { L.LocToken _ L.SEQUENCES }
    SERIALIZABLE { L.LocToken _ L.SERIALIZABLE }
    SERVER { L.LocToken _ L.SERVER }
    SESSION { L.LocToken _ L.SESSION }
    SESSION_USER { L.LocToken _ L.SESSION_USER }
    SETOF { L.LocToken _ L.SETOF }
    SETS { L.LocToken _ L.SETS }
    SHARE { L.LocToken _ L.SHARE }
    SHOW { L.LocToken _ L.SHOW }
    SIMILAR { L.LocToken _ L.SIMILAR }
    SIMPLE { L.LocToken _ L.SIMPLE }
    SKIP { L.LocToken _ L.SKIP }
    SMALLINT { L.LocToken _ L.SMALLINT }
    SNAPSHOT { L.LocToken _ L.SNAPSHOT }
    SOME { L.LocToken _ L.SOME }
    SQL_P { L.LocToken _ L.SQL_P }
    STABLE { L.LocToken _ L.STABLE }
    STANDALONE_P { L.LocToken _ L.STANDALONE_P }
    START { L.LocToken _ L.START }
    STATEMENT { L.LocToken _ L.STATEMENT }
    STATISTICS { L.LocToken _ L.STATISTICS }
    STDIN { L.LocToken _ L.STDIN }
    STDOUT { L.LocToken _ L.STDOUT }
    STORAGE { L.LocToken _ L.STORAGE }
    STORED { L.LocToken _ L.STORED }
    STRICT_P { L.LocToken _ L.STRICT_P }
    STRIP_P { L.LocToken _ L.STRIP_P }
    SUBSCRIPTION { L.LocToken _ L.SUBSCRIPTION }
    SUBSTRING { L.LocToken _ L.SUBSTRING }
    SUPPORT { L.LocToken _ L.SUPPORT }
    SYMMETRIC { L.LocToken _ L.SYMMETRIC }
    SYSID { L.LocToken _ L.SYSID }
    SYSTEM_P { L.LocToken _ L.SYSTEM_P }
    TABLE { L.LocToken _ L.TABLE }
    TABLES { L.LocToken _ L.TABLES }
    TABLESAMPLE { L.LocToken _ L.TABLESAMPLE }
    TABLESPACE { L.LocToken _ L.TABLESPACE }
    TEMP { L.LocToken _ L.TEMP }
    TEMPLATE { L.LocToken _ L.TEMPLATE }
    TEMPORARY { L.LocToken _ L.TEMPORARY }
    TEXT_P { L.LocToken _ L.TEXT_P }
    THEN { L.LocToken _ L.THEN }
    TIES { L.LocToken _ L.TIES }
    TIME { L.LocToken _ L.TIME }
    TIMESTAMP { L.LocToken _ L.TIMESTAMP }
    TO { L.LocToken _ L.TO }
    TRAILING { L.LocToken _ L.TRAILING }
    TRANSACTION { L.LocToken _ L.TRANSACTION }
    TRANSFORM { L.LocToken _ L.TRANSFORM }
    TREAT { L.LocToken _ L.TREAT }
    TRIGGER { L.LocToken _ L.TRIGGER }
    TRIM { L.LocToken _ L.TRIM }
    TRUE_P { L.LocToken _ L.TRUE_P }
    TRUNCATE { L.LocToken _ L.TRUNCATE }
    TRUSTED { L.LocToken _ L.TRUSTED }
    TYPES_P { L.LocToken _ L.TYPES_P }
    TYPE_P { L.LocToken _ L.TYPE_P }
    UNBOUNDED { L.LocToken _ L.UNBOUNDED }
    UNCOMMITTED { L.LocToken _ L.UNCOMMITTED }
    UNENCRYPTED { L.LocToken _ L.UNENCRYPTED }
    UNIQUE { L.LocToken _ L.UNIQUE }
    UNKNOWN { L.LocToken _ L.UNKNOWN }
    UNLISTEN { L.LocToken _ L.UNLISTEN }
    UNLOGGED { L.LocToken _ L.UNLOGGED }
    UNTIL { L.LocToken _ L.UNTIL }
    USER { L.LocToken _ L.USER }
    VACUUM { L.LocToken _ L.VACUUM }
    VALID { L.LocToken _ L.VALID }
    VALIDATE { L.LocToken _ L.VALIDATE }
    VALIDATOR { L.LocToken _ L.VALIDATOR }
    VALUE_P { L.LocToken _ L.VALUE_P }
    VARCHAR { L.LocToken _ L.VARCHAR }
    VARIADIC { L.LocToken _ L.VARIADIC }
    VARYING { L.LocToken _ L.VARYING }
    VERBOSE { L.LocToken _ L.VERBOSE }
    VERSION_P { L.LocToken _ L.VERSION_P }
    VIEW { L.LocToken _ L.VIEW }
    VIEWS { L.LocToken _ L.VIEWS }
    VOLATILE { L.LocToken _ L.VOLATILE }
    WHEN { L.LocToken _ L.WHEN }
    WHITESPACE_P { L.LocToken _ L.WHITESPACE_P }
    WINDOW { L.LocToken _ L.WINDOW }
    WITH { L.LocToken _ L.WITH }
    WITHIN { L.LocToken _ L.WITHIN }
    WITHOUT { L.LocToken _ L.WITHOUT }
    WORK { L.LocToken _ L.WORK }
    WRAPPER { L.LocToken _ L.WRAPPER }
    WRITE { L.LocToken _ L.WRITE }
    XMLATTRIBUTES { L.LocToken _ L.XMLATTRIBUTES }
    XMLCONCAT { L.LocToken _ L.XMLCONCAT }
    XMLELEMENT { L.LocToken _ L.XMLELEMENT }
    XMLEXISTS { L.LocToken _ L.XMLEXISTS }
    XMLFOREST { L.LocToken _ L.XMLFOREST }
    XMLNAMESPACES { L.LocToken _ L.XMLNAMESPACES }
    XMLPARSE { L.LocToken _ L.XMLPARSE }
    XMLPI { L.LocToken _ L.XMLPI }
    XMLROOT { L.LocToken _ L.XMLROOT }
    XMLSERIALIZE { L.LocToken _ L.XMLSERIALIZE }
    XMLTABLE { L.LocToken _ L.XMLTABLE }
    XML_P { L.LocToken _ L.XML_P }
    YEAR_P { L.LocToken _ L.YEAR_P }
    YES_P { L.LocToken _ L.YES_P }
    ZONE { L.LocToken _ L.ZONE }

%%

-- Unlike gram.y, we only parse a single statement, and only DML.

Statement :: { Statement }
    : PreparableStmt { $1 }
    | PreparableStmt SEMICOLON { $1 }

PreparableStmt :: { Statement }
    : SelectStmt { QS $1 }
    | DeleteStmt { QD $1 }
    | Insert { QI $1 }
    | Update { QU $1 }

returning_clause :: { [ResTarget] }
    : RETURNING target_list { $2 }
    | { [] }

DeleteStmt :: { Delete }
    : opt_with_clause DELETE FROM relation_expr_opt_alias
			using_clause where_or_current_clause returning_clause
      { Delete
      { withClause = $1
      , table = $4
      , using = $5
      , whereClause = $6
      , returningList = $7
      } }

using_clause :: { [TableRef] }
    : USING from_list { $2 }
    | { [] }

-- * A complete SELECT statement looks like this.
-- *
-- * The rule returns either a single SelectStmt node or a tree of them,
-- * representing a set-operation tree.
-- *
-- * There is an ambiguity when a sub-SELECT is within an a_expr and there
-- * are excess parentheses: do the parentheses belong to the sub-SELECT or
-- * to the surrounding a_expr?  We don't really care, but bison wants to know.
-- * To resolve the ambiguity, we are careful to define the grammar so that
-- * the decision is staved off as long as possible: as long as we can keep
-- * absorbing parentheses into the sub-SELECT, we will do so, and only when
-- * it's no longer possible to do that will we decide that parens belong to
-- * the expression.	For example, in "SELECT (((SELECT 2)) + 3)" the extra
-- * parentheses are treated as part of the sub-select.  The necessity of doing
-- * it that way is shown by "SELECT (((SELECT 2)) UNION SELECT 2)".	Had we
-- * parsed "((SELECT 2))" as an a_expr, it'd be too late to go back to the
-- * SELECT viewpoint when we see the UNION.
-- *
-- * This approach is implemented by defining a nonterminal select_with_parens,
-- * which represents a SELECT with at least one outer layer of parentheses,
-- * and being careful to use select_with_parens, never '(' SelectStmt ')',
-- * in the expression grammar.  We will then have shift-reduce conflicts
-- * which we can resolve in favor of always treating '(' <select> ')' as
-- * a select_with_parens.  To resolve the conflicts, the productions that
-- * conflict with the select_with_parens productions are manually given
-- * precedences lower than the precedence of ')', thereby ensuring that we
-- * shift ')' (and then reduce to select_with_parens) rather than trying to
-- * reduce the inner <select> nonterminal to something else.  We use UMINUS
-- * precedence for this, which is a fairly arbitrary choice.
-- *
-- * To be able to define select_with_parens itself without ambiguity, we need
-- * a nonterminal select_no_parens that represents a SELECT structure with no
-- * outermost parentheses.  This is a little bit tedious, but it works.
-- *
-- * In non-expression contexts, we use SelectStmt which can represent a SELECT
-- * with or without outer parentheses.

SelectStmt :: { SelectStmt }
    : select_no_parens %prec UMINUS { $1 }
    | select_with_parens %prec UMINUS { $1 }

select_with_parens :: { SelectStmt }
    : '(' select_no_parens ')' { $2 }
    | '(' select_with_parens ')' { $2 }

-- *  This rule parses the equivalent of the standard's <query expression>.
-- *  The duplicative productions are annoying, but hard to get rid of without
-- *  creating shift/reduce conflicts.
-- *
-- * 	The locking clause (FOR UPDATE etc) may be before or after LIMIT/OFFSET.
-- * 	In <=7.2.X, LIMIT/OFFSET had to be after FOR UPDATE
-- * 	We now support both orderings, but prefer LIMIT/OFFSET before the locking
-- *  clause.
-- * 	2002-08-28 bjm

select_no_parens :: { SelectStmt }
    : simple_select { $1 }
    | select_clause sort_clause { S $1 selectOptions { sortBy = $2 } }
    | select_clause opt_sort_clause for_locking_clause opt_select_limit
        { S $1 selectOptions { sortBy = $2, locking = $3, offset = fst $4, limit = snd $4 } }
    | select_clause opt_sort_clause select_limit opt_for_locking_clause
        { S $1 selectOptions { sortBy = $2, offset = fst $3, limit = snd $3, locking = $4 } }
    | with_clause select_clause { S $2 selectOptions { withClause = Just $1 } }
    | with_clause select_clause sort_clause { S $2 selectOptions { withClause = Just $1, sortBy = $3 } }
    | with_clause select_clause opt_sort_clause for_locking_clause opt_select_limit
        { S $2 selectOptions { withClause = Just $1, sortBy = $3, locking = $4, offset = fst $5, limit = snd $5 } }
    | with_clause select_clause opt_sort_clause select_limit opt_for_locking_clause
        { S $2 selectOptions { withClause = Just $1, sortBy = $3, offset = fst $4, limit = snd $4, locking = $5 } }

select_clause :: { SelectStmt }
    : simple_select                            { $1 }
    | select_with_parens                    { $1 }

-- * This rule parses SELECT statements that can appear within set operations,
-- * including UNION, INTERSECT and EXCEPT.  '(' and ')' can be used to specify
-- * the ordering of the set operations.	Without '(' and ')' we want the
-- * operations to be ordered per the precedence specs at the head of this file.
-- *
-- * As with select_no_parens, simple_select cannot have outer parentheses,
-- * but can have parenthesized subclauses.
-- *
-- * Note that sort clauses cannot be included at this level -- *- SQL requires
-- *		SELECT foo UNION SELECT bar ORDER BY baz
-- * to be parsed as
-- *		(SELECT foo UNION SELECT bar) ORDER BY baz
-- * not
-- *		SELECT foo UNION (SELECT bar ORDER BY baz)
-- * Likewise for WITH, FOR UPDATE and LIMIT.  Therefore, those clauses are
-- * described as part of the select_no_parens production, not simple_select.
-- * This does not limit functionality, because you can reintroduce these
-- * clauses inside parentheses.
-- *
-- * NOTE: only the leftmost component SelectStmt should have INTO.
-- * However, this is not checked by the grammar; parse analysis must check it.

-- TODO include into_clause
simple_select :: { SelectStmt }
    : SELECT opt_all_clause opt_target_list
           into_clause from_clause where_clause
           group_clause having_clause window_clause { Simple (Select
                { distinct = Nothing
                , targetList = $3
                , from = $5
                , whereClause = $6
                , groupBy = $7
                , having = $8
                , window = $9
                })
           }
    | SELECT distinct_clause target_list
        into_clause from_clause where_clause
        group_clause having_clause window_clause
      { Simple (Select
        { distinct = Just $2
        , targetList = $3
        -- TODO into
        , from = $5
        , whereClause = $6
        , groupBy = $7
        , having = $8
        , window = $9
        }) }
            | values_clause { SelectValues (NE.reverse $1) }
            | TABLE relation_expr { Simple select { targetList = [ Star ], from = [ J (Table $2) ] } }
            -- * same as SELECT * FROM relation_expr
            | select_clause UNION all_or_distinct select_clause { Set Union $3 $1 $4 }
            | select_clause INTERSECT all_or_distinct select_clause { Set Intersect $3 $1 $4 }
            | select_clause EXCEPT all_or_distinct select_clause { Set Except $3 $1 $4 }

-- * SQL standard WITH clause looks like:
-- *
-- * WITH [ RECURSIVE ] <query name> [ (<column>,...) ]
-- *		AS (query) [ SEARCH or CYCLE clause ]
-- *
-- * We don't currently support the SEARCH or CYCLE clause.

-- * Recognizing WITH_LA here allows a CTE to be named TIME or ORDINALITY.
with_clause
    : WITH cte_list { With $2 NotRecursive }
    -- FIXME do we need WITH_LA ?
    | WITH RECURSIVE cte_list { With $3 Recursive }

cte_list : list(common_table_expr) { reverse $1 }

common_table_expr
  :  name opt_name_list AS opt_materialized '(' PreparableStmt ')'
  { CommonTableExpr
    { name = $1
    , aliases = $2
    , materialized = $4
    , query = $6
    }
  }

opt_materialized :: { Materialized }
    : MATERIALIZED { Materialized }
    | NOT MATERIALIZED { NotMaterialized }
    | { MaterializeDefault }

opt_with_clause :: { Maybe WithClause }
    : with_clause { Just $1 }
    | { Nothing }

into_clause:
			-- TODO INTO OptTempTableName
			-- TODO 	{
			-- TODO 		$$ = makeNode(IntoClause);
			-- TODO 		$$->rel = $2;
			-- TODO 		$$->colNames = NIL;
			-- TODO 		$$->options = NIL;
			-- TODO 		$$->onCommit = ONCOMMIT_NOOP;
			-- TODO 		$$->tableSpaceName = NULL;
			-- TODO 		$$->viewQuery = NULL;
			-- TODO 		$$->skipData = false;
				-- }
    { Nothing }

-- TODO OptTempTableName:

opt_table :: { () }
    : TABLE { () }
    | { () }

all_or_distinct :: { AllOrDistinct }
    : ALL { All }
    | DISTINCT { Distinct }
    | { Distinct }

-- * We use (DistinctAll) as a placeholder to indicate that all target expressions
-- * should be placed in the DISTINCT list during parsetree analysis.
distinct_clause :: { DistinctClause }
    : DISTINCT { DistinctAll }
    | DISTINCT ON '(' expr_list ')' { DistinctOn (NE.fromList $4) }

opt_all_clause
    : ALL { () }
    | { () }

opt_sort_clause :: { [SortBy] }
    : sort_clause { $1 }
    | { [] }

sort_clause :: { [SortBy] }
    : ORDER BY sortby_list { reverse $3 }

sortby_list : list(sortby) { $1 }

sortby :: { SortBy }
    : a_expr USING qual_all_Op opt_nulls_order { SortBy $1 (SortUsing $3) $4 }
    | a_expr opt_asc_desc opt_nulls_order { SortBy $1 (SortOrder $2) $3 }

select_limit :: { (Maybe Expr, Maybe Expr) }
    : limit_clause offset_clause { (Just $2, Just $1) }
    | offset_clause limit_clause { (Just $1, Just $2) }
    | limit_clause { (Nothing, Just $1) }
    | offset_clause { (Just $1, Nothing) }

opt_select_limit :: { (Maybe Expr, Maybe Expr) }
    : select_limit { $1 }
    | { (Nothing, Nothing) }

limit_clause :: { Expr }
    : LIMIT select_limit_value { $2 }
        -- * Disabled because it was too confusing, bjm 2002-02-18 */
    | LIMIT select_limit_value ',' select_offset_value
        {% fail "LIMIT #,# syntax is not supported.  Use separate LIMIT and OFFSET clauses." } -- error message to match Postgres
        -- * SQL:2008 syntax */
        -- * to avoid shift/reduce conflicts, handle the optional value with
        -- * a separate production rather than an opt_ expression.  The fact
        -- * that ONLY is fully reserved means that this way, we defer any
        -- * decision about what rule reduces ROW or ROWS to the point where
        -- * we can see the ONLY token in the lookahead slot.
    | FETCH first_or_next select_fetch_first_value row_or_rows ONLY { $3 }
    | FETCH first_or_next row_or_rows ONLY { Lit (I 1) }

offset_clause :: { Expr }
    : OFFSET select_offset_value { $2 }
    -- * SQL:2008 syntax */
    | OFFSET select_fetch_first_value row_or_rows { $2 }

select_limit_value :: { Expr }
    : a_expr { $1 }
    -- * LIMIT ALL is represented as a NULL constant */
    | ALL { Lit Null }

select_offset_value :: { Expr }
    : a_expr { $1 }

-- * Allowing full expressions without parentheses causes various parsing
-- * problems with the trailing ROW/ROWS key words.  SQL spec only calls for
-- * <simple value specification>, which is either a literal or a parameter (but
-- * an <SQL parameter reference> could be an identifier, bringing up conflicts
-- * with ROW/ROWS). We solve this by leveraging the presence of ONLY (see above)
-- * to determine whether the expression is missing rather than trying to make it
-- * optional in this rule.
-- *
-- * c_expr covers almost all the spec-required cases (and more), but it doesn't
-- * cover signed numeric literals, which are allowed by the spec. So we include
-- * those here explicitly. We need FCONST as well as ICONST because values that
-- * don't fit in the platform's "long", but do fit in bigint, should still be
-- * accepted here. (This is possible in 64-bit Windows as well as all 32-bit
-- * builds.)

select_fetch_first_value :: { Expr }
    : c_expr { $1 }
    | '+' I_or_F_const { Lit $2 }
    | '-' I_or_F_const { Unary Negate (Lit $2) }

I_or_F_const :: { Literal }
    : Fconst { F $1 }
    | Iconst { I $1 }

-- * noise words
row_or_rows :: { () }
  : ROW { () }
  | ROWS { () }

first_or_next :: { () }
  : FIRST_P { () }
  | NEXT { () }

-- * This syntax for group_clause tries to follow the spec quite closely.
-- * However, the spec allows only column references, not expressions,
-- * which introduces an ambiguity between implicit row constructors
-- * (a,b) and lists of column references.
-- *
-- * We handle this by using the a_expr production for what the spec calls
-- * <ordinary grouping set>, which in the spec represents either one column
-- * reference or a parenthesized list of column references. Then, we check the
-- * top node of the a_expr to see if it's an implicit RowExpr, and if so, just
-- * grab and use the list, discarding the node. (this is done in parse analysis,
-- * not here)
-- *
-- * (we abuse the row_format field of RowExpr to distinguish implicit and
-- * explicit row constructors; it's debatable if anyone sanely wants to use them
-- * in a group clause, but if they have a reason to, we make it possible.)
-- *
-- * Each item in the group_clause list is either an expression tree or a
-- * GroupingSet node of some type.
group_clause :: { [Expr] }
    : GROUP_P BY group_by_list { reverse $3 }
    | { [] }

group_by_list : list(group_by_item) { $1 }

group_by_item
    : a_expr { $1 }
-- TODO 			| empty_grouping_set					{ $$ = $1; }
-- TODO 			| cube_clause							{ $$ = $1; }
-- TODO 			| rollup_clause							{ $$ = $1; }
-- TODO 			| grouping_sets_clause					{ $$ = $1; }

-- TODO empty_grouping_set:
-- TODO 			'(' ')'
-- TODO 				{
-- TODO 					$$ = (Node *) makeGroupingSet(GROUPING_SET_EMPTY, NIL, @1);

-- TODO rollup_clause:
-- TODO cube_clause:
-- TODO grouping_sets_clause:

having_clause :: { Maybe Expr }
    : HAVING a_expr { Just $2 }
    | { Nothing }

for_locking_clause :: { [Locking] }
    : for_locking_items { reverse $1 }
    | FOR READ ONLY { [] }

opt_for_locking_clause :: { [Locking] }
    : for_locking_clause { $1 }
    | { [] }

for_locking_items :: { [Locking] }
    : for_locking_item { [$1] }
    | for_locking_items for_locking_item { $2 : $1 }

for_locking_item :: { Locking }
    : for_locking_strength locked_rels_list opt_nowait_or_skip
    { Locking $1 (reverse $2) $3 }

for_locking_strength :: { LockingStrength }
    : FOR UPDATE  { ForUpdate }
    | FOR NO KEY UPDATE  { ForNoKeyUpdate }
    | FOR SHARE  { ForShare }
    | FOR KEY SHARE  { ForKeyShare }

locked_rels_list :: { [Text] }
    : OF qualified_name_list { $2 }
    | { [] }

opt_nowait_or_skip :: { LockWait }
    : NOWAIT { LockWaitError }
    | SKIP LOCKED { LockWaitSkip }
    | { LockWaitBlock }

-- * We should allow ROW '(' expr_list ')' too, but that seems to require
-- * making VALUES a fully reserved word, which will probably break more apps
-- * than allowing the noise-word is worth.
values_clause :: { NE.NonEmpty (NE.NonEmpty Expr) }
    : VALUES '(' expr_list ')' { NE.fromList $3 :| [] }
    | values_clause ',' '(' expr_list ')' { NE.cons (NE.fromList $4) $1 }

 -- *	clauses common to all Optimizable Stmts:
 -- *		from_clause		- allow list of both JOIN expressions and table names
 -- *		where_clause	- qualifications for joins or restrictions

from_clause :: { [TableRef] }
    : FROM from_list { reverse $2 }
    | { [] }

from_list : list(table_ref) { $1 }

-- * table_ref is where an alias clause can be attached.
table_ref :: { TableRef }
    : relation_expr opt_alias_clause { case $2 of
        Nothing -> J (Table $1)
        Just a -> As (Table $1) a }
-- TODO			| relation_expr opt_alias_clause tablesample_clause
-- TODO				{
-- TODO					RangeTableSample *n = (RangeTableSample *) $3;
-- TODO					$1->alias = $2;
-- TODO					/* relation_expr goes inside the RangeTableSample node */
-- TODO					n->relation = (Node *) $1;
-- TODO					$$ = (Node *) n;
-- TODO				}
-- TODO			| func_table func_alias_clause
-- TODO				{
-- TODO					RangeFunction *n = (RangeFunction *) $1;
-- TODO					n->alias = linitial($2);
-- TODO					n->coldeflist = lsecond($2);
-- TODO					$$ = (Node *) n;
-- TODO				}
-- TODO			| LATERAL_P func_table func_alias_clause
-- TODO				{
-- TODO					RangeFunction *n = (RangeFunction *) $2;
-- TODO					n->lateral = true;
-- TODO					n->alias = linitial($3);
-- TODO					n->coldeflist = lsecond($3);
-- TODO					$$ = (Node *) n;
-- TODO				}
-- TODO			| xmltable opt_alias_clause
-- TODO				{
-- TODO					RangeTableFunc *n = (RangeTableFunc *) $1;
-- TODO					n->alias = $2;
-- TODO					$$ = (Node *) n;
-- TODO				}
-- TODO			| LATERAL_P xmltable opt_alias_clause
-- TODO				{
-- TODO					RangeTableFunc *n = (RangeTableFunc *) $2;
-- TODO					n->lateral = true;
-- TODO					n->alias = $3;
-- TODO					$$ = (Node *) n;
-- TODO				}
-- * The SQL spec does not permit a subselect
-- * (<derived_table>) without an alias clause,
-- * so we don't either.  This avoids the problem
-- * of needing to invent a unique refname for it.
-- * That could be surmounted if there's sufficient
-- * popular demand, but for now let's just implement
-- * the spec and see if anyone complains.
-- * However, it does seem like a good idea to emit
-- * an error message that's better than "syntax error".

-- The comment above and the error messages below are straight from Postgres
    | select_with_parens opt_alias_clause {%
        case $2 of
            Just alias -> return (SubSelect $1 alias)
            Nothing -> case $1 of
                SelectValues _ -> fail "VALUES in FROM must have an alias"

                _ -> fail "subquery in FROM must have an alias"
    }
-- TODO					$$ = (Node *) n;
-- TODO				}
-- TODO			| LATERAL_P select_with_parens opt_alias_clause
-- TODO				{
-- TODO					RangeSubselect *n = makeNode(RangeSubselect);
-- TODO					n->lateral = true;
-- TODO					n->subquery = $2;
-- TODO					n->alias = $3;
-- TODO					/* same comment as above */
-- TODO					if ($3 == NULL)
-- TODO					{
-- TODO						if (IsA($2, SelectStmt) &&
-- TODO							((SelectStmt *) $2)->valuesLists)
-- TODO							ereport(ERROR,
-- TODO									(errcode(ERRCODE_SYNTAX_ERROR),
-- TODO									 errmsg("VALUES in FROM must have an alias"),
-- TODO									 errhint("For example, FROM (VALUES ...) [AS] foo."),
-- TODO									 parser_errposition(@2)));
-- TODO						else
-- TODO							ereport(ERROR,
-- TODO									(errcode(ERRCODE_SYNTAX_ERROR),
-- TODO									 errmsg("subquery in FROM must have an alias"),
-- TODO									 errhint("For example, FROM (SELECT ...) [AS] foo."),
-- TODO									 parser_errposition(@2)));
-- TODO					}
-- TODO					$$ = (Node *) n;
-- TODO				}
    | joined_table { J $1 }
    | '(' joined_table ')' alias_clause { As $2 $4 }

-- * It may seem silly to separate joined_table from table_ref, but there is
-- * method in SQL's madness: if you don't do it this way you get reduce-
-- * reduce conflicts, because it's not clear to the parser generator whether
-- * to expect alias_clause after ')' or not.  For the same reason we must
-- * treat 'JOIN' and 'join_type JOIN' separately, rather than allowing
-- * join_type to expand to empty; if we try it, the parser generator can't
-- * figure out when to reduce an empty join_type right after table_ref.
-- *
-- * Note that a CROSS JOIN is the same as an unqualified
-- * INNER JOIN, and an INNER JOIN/ON has the same shape
-- * but a qualification expression to limit membership.
-- * A NATURAL JOIN implicitly matches column names between
-- * tables and the shape is determined by which columns are
-- * in common. We'll collect columns during the later transformations.

joined_table :: { JoinedTable }
    : '(' joined_table ')' { $2 }
    -- * CROSS JOIN is same as unqualified inner join
    | table_ref CROSS JOIN table_ref { CrossJoin $1 $4 }
    | table_ref join_type JOIN table_ref join_qual { Join $2 $5 $1 $4 }
    | table_ref JOIN table_ref join_qual { Join Inner $4 $1 $3 }
    | table_ref NATURAL join_type JOIN table_ref { Join $3 Natural $1 $5 }
    | table_ref NATURAL JOIN table_ref { Join Inner Natural $1 $4 }

alias_clause :: { Alias }
    : AS ColId '(' name_list ')' { Alias $2 (reverse $4) }
    | AS ColId { Alias $2 [] }
    | ColId '(' name_list ')' { Alias $1 (reverse $3) }
    | ColId { Alias $1 [] }

opt_alias_clause :: { Maybe Alias }
    : alias_clause { Just $1 }
    | { Nothing }

-- TODO func_alias_clause:

join_type :: { JoinType }
    : FULL join_outer { Full }
    | LEFT join_outer { LeftJoin }
    | RIGHT join_outer { RightJoin }
    | INNER_P { Inner }

join_outer :: { () } -- * OUTER is just noise...
    : OUTER_P { () }
    | { () }

-- * JOIN qualification clauses
-- * Possibilities are:
-- * USING ( column list ) allows only unqualified column names,
-- *   which must match between tables.
-- * ON expr allows more general qualifications.
join_qual :: { JoinQual }
    : USING '(' name_list ')' { Using (reverse $3) }
    | ON a_expr { On $2 }

relation_expr :: { Name }
    : qualified_name { mkName $1 } -- * inheritance query, implicitly
    | qualified_name '*' { Name $1 True Nothing } -- * inheritance query, explicitly
    | ONLY qualified_name { Name $2 False Nothing } -- * no inheritance
    | ONLY '(' qualified_name ')' { Name $3 False Nothing } -- * no inheritance, SQL99-style syntax

relation_expr_list : list(relation_expr) { $1 }

-- * Given "UPDATE foo set set ...", we have to decide without looking any
-- * further ahead whether the first "set" is an alias or the UPDATE's SET
-- * keyword.  Since "set" is allowed as a column name both interpretations
-- * are feasible.  We resolve the shift/reduce conflict by giving the first
-- * relation_expr_opt_alias production a higher precedence than the SET token
-- * has, causing the parser to prefer to reduce, in effect assuming that the
-- * SET is not an alias.
relation_expr_opt_alias :: { Name }
    : relation_expr %prec UMINUS { $1 }
    | relation_expr ColId { $1 { alias = Just $2 } }
    | relation_expr AS ColId { $1 { alias = Just $3 } }

-- TODO tablesample_clause
-- TODO opt_repeatable_clause:
-- TODO func_table
-- TODO rowsfrom_item
-- TODO rowsfrom_list:
-- TODO opt_col_def_list
-- TODO opt_ordinality

where_clause :: { Maybe Expr }
    : WHERE a_expr { Just $2 }
    | { Nothing }

where_or_current_clause :: { Maybe WhereOrCurrent }
: WHERE a_expr { Just (Where $2) }
| WHERE CURRENT_P OF cursor_name { Just (Current $4) }
| { Nothing }

cursor_name :: { Text }
    : name { $1 }

-- TODO OptTableFuncElementList ::
-- TODO TableFuncElementList ::
-- TODO TableFuncElement ::
-- TODO xmltable ::
-- TODO xmltable_column_list ::
-- TODO xmltable_column_el ::
-- TODO xmltable_column_option_list ::
-- TODO xmltable_column_option_el ::
-- TODO xml_namespace_list ::
-- TODO xml_namespace_el ::
-- TODO Typename ::
-- TODO opt_array_bounds ::
-- TODO SimpleTypename ::
-- TODO ConstTypename ::
-- TODO GenericType ::
-- TODO opt_type_modifiers ::
-- TODO Numeric ::

-- *	expression grammar

-- * General expressions
-- * This is the heart of the expression syntax.
-- *
-- * We have two expression types: a_expr is the unrestricted kind, and
-- * b_expr is a subset that must be used in some places to avoid shift/reduce
-- * conflicts.  For example, we can't do BETWEEN as "BETWEEN a_expr AND a_expr"
-- * because that use of AND conflicts with AND as a boolean operator.  So,
-- * b_expr is used in BETWEEN and we remove boolean keywords from b_expr.
-- *
-- * Note that '(' a_expr ')' is a b_expr, so an unrestricted expression can
-- * always be used by surrounding it with parens.
-- *
-- * c_expr is all the productions that are common to a_expr and b_expr;
-- * it's factored out just to eliminate redundant coding.
-- *
-- * Be careful of productions involving more than one terminal token.
-- * By default, bison will assign such productions the precedence of their
-- * last terminal, but in nearly all cases you want it to be the precedence
-- * of the first terminal instead; otherwise you will not get the behavior
-- * you expect!  So we use %prec annotations freely to set precedences.

a_expr :: { Expr }
    : c_expr { $1 }
-- TODO 			| a_expr TYPECAST Typename
-- TODO 					{ $$ = makeTypeCast($1, $3, @2); }
-- TODO 			| a_expr COLLATE any_name
-- TODO 				{
-- TODO 					CollateClause *n = makeNode(CollateClause);
-- TODO 					n->arg = $1;
-- TODO 					n->collname = $3;
-- TODO 					n->location = @2;
-- TODO 					$$ = (Node *) n;
-- TODO 				}
-- TODO 			| a_expr AT TIME ZONE a_expr			%prec AT
-- TODO 				{
-- TODO 					$$ = (Node *) makeFuncCall(SystemFuncName("timezone"),
-- TODO 											   list_make2($5, $1),
-- TODO 											   @2);
-- What about lines 13060-13102 of gram.y?
    | '+' a_expr					%prec UMINUS { $2 } -- TODO keep + for round-trip?
    | '-' a_expr					%prec UMINUS { Unary Negate $2 }
    | a_expr '+' a_expr { BinOp Add $1 $3 }
    | a_expr '-' a_expr { BinOp Sub $1 $3 }
    | a_expr '*' a_expr { BinOp Mul $1 $3 }
    | a_expr '/' a_expr { BinOp Div $1 $3 }
    | a_expr '%' a_expr { BinOp Mod $1 $3 }
    | a_expr '^' a_expr { BinOp Exponent $1 $3 }
    | a_expr '<' a_expr { BinOp LT $1 $3 }
    | a_expr '>' a_expr { BinOp GT $1 $3 }
    | a_expr '=' a_expr { BinOp Eq $1 $3 }
    | a_expr '<=' a_expr { BinOp LTE $1 $3 }
    | a_expr '>=' a_expr { BinOp GTE $1 $3 }
    | a_expr '!=' a_expr { BinOp NEq $1 $3 }
-- TODO 			| a_expr qual_Op a_expr				%prec Op
-- TODO 				{ $$ = (Node *) makeA_Expr(AEXPR_OP, $2, $1, $3, @2); }
-- TODO 			| qual_Op a_expr					%prec Op
-- TODO 				{ $$ = (Node *) makeA_Expr(AEXPR_OP, $1, NULL, $2, @1); }
-- TODO 			| a_expr qual_Op					%prec POSTFIXOP
-- TODO 				{ $$ = (Node *) makeA_Expr(AEXPR_OP, $2, $1, NULL, @2); }
-- TODO     | a_expr qual_Op a_expr				%prec Op { BinOp $2 $1 $3 }
    | a_expr AND a_expr { BinOp And $1 $3 }
	  | a_expr OR a_expr { BinOp Or $1 $3 }
    | NOT a_expr { Unary Not $2 }
-- TODO 			| NOT a_expr						%prec NOT
-- TODO 				{ $$ = makeNotExpr($2, @1); }
   | a_expr LIKE a_expr { L (LikeE Like $1 $3 Nothing False) }
   | a_expr LIKE a_expr ESCAPE a_expr %prec LIKE { L (LikeE Like $1 $3 (Just $5) False) }
   | a_expr NOT LIKE a_expr %prec NOT { L (LikeE Like $1 $4 Nothing True) }
   | a_expr NOT LIKE a_expr ESCAPE a_expr %prec NOT
        { L (LikeE Like $1 $4 (Just $6) True) }
    | a_expr ILIKE a_expr { L (LikeE ILike $1 $3 Nothing False) }
    | a_expr ILIKE a_expr ESCAPE a_expr %prec ILIKE { L (LikeE ILike $1 $3 (Just $5) False) }
    | a_expr NOT ILIKE a_expr %prec NOT { L (LikeE ILike $1 $4 Nothing True) }
    | a_expr NOT ILIKE a_expr ESCAPE a_expr %prec NOT
        { L (LikeE ILike $1 $4 (Just $6) True) }
    | a_expr SIMILAR TO a_expr %prec SIMILAR
        { L (LikeE Similar $1 $4 Nothing False) }
    | a_expr SIMILAR TO a_expr ESCAPE a_expr %prec SIMILAR
        { L (LikeE Similar $1 $4 (Just $6) False) }
    | a_expr NOT SIMILAR TO a_expr %prec NOT
        { L (LikeE Similar $1 $5 Nothing True) }
    | a_expr NOT SIMILAR TO a_expr ESCAPE a_expr %prec NOT
        { L (LikeE Similar $1 $5 (Just $7) True) }
-- * NullTest clause
-- * Define SQL-style Null test clause.
-- * Allow two forms described in the standard:
-- *	a IS NULL
-- *	a IS NOT NULL
-- * Allow two SQL extensions
-- *	a ISNULL
-- *	a NOTNULL
    | a_expr IS NULL_P %prec IS { Unary IsNull $1 }
    | a_expr ISNULL { Unary IsNull $1 }
    | a_expr IS NOT NULL_P %prec IS { Unary NotNull $1 }
    | a_expr NOTNULL { Unary NotNull $1 }
-- TODO | row OVERLAPS row
-- TODO {
-- TODO if (list_length($1) != 2)
-- TODO ereport(ERROR,
-- TODO (errcode(ERRCODE_SYNTAX_ERROR),
-- TODO errmsg("wrong number of parameters on left side of OVERLAPS expression"),
-- TODO parser_errposition(@1)));
-- TODO if (list_length($3) != 2)
-- TODO ereport(ERROR,
-- TODO (errcode(ERRCODE_SYNTAX_ERROR),
-- TODO errmsg("wrong number of parameters on right side of OVERLAPS expression"),
-- TODO parser_errposition(@3)));
-- TODO $$ = (Node *) makeFuncCall(SystemFuncName("overlaps"),
-- TODO list_concat($1, $3),
-- TODO @2);
-- TODO }
-- TODO | a_expr IS TRUE_P							%prec IS
-- TODO {
-- TODO BooleanTest *b = makeNode(BooleanTest);
-- TODO b->arg = (Expr *) $1;
-- TODO b->booltesttype = IS_TRUE;
-- TODO b->location = @2;
-- TODO $$ = (Node *)b;
-- TODO }
-- TODO | a_expr IS NOT TRUE_P						%prec IS
-- TODO {
-- TODO BooleanTest *b = makeNode(BooleanTest);
-- TODO b->arg = (Expr *) $1;
-- TODO b->booltesttype = IS_NOT_TRUE;
-- TODO b->location = @2;
-- TODO $$ = (Node *)b;
-- TODO }
-- TODO | a_expr IS FALSE_P							%prec IS
-- TODO {
-- TODO BooleanTest *b = makeNode(BooleanTest);
-- TODO b->arg = (Expr *) $1;
-- TODO b->booltesttype = IS_FALSE;
-- TODO b->location = @2;
-- TODO $$ = (Node *)b;
-- TODO }
-- TODO | a_expr IS NOT FALSE_P						%prec IS
-- TODO {
-- TODO BooleanTest *b = makeNode(BooleanTest);
-- TODO b->arg = (Expr *) $1;
-- TODO b->booltesttype = IS_NOT_FALSE;
-- TODO b->location = @2;
-- TODO $$ = (Node *)b;
-- TODO }
-- TODO | a_expr IS UNKNOWN							%prec IS
-- TODO {
-- TODO BooleanTest *b = makeNode(BooleanTest);
-- TODO b->arg = (Expr *) $1;
-- TODO b->booltesttype = IS_UNKNOWN;
-- TODO b->location = @2;
-- TODO $$ = (Node *)b;
-- TODO }
-- TODO | a_expr IS NOT UNKNOWN						%prec IS
-- TODO {
-- TODO BooleanTest *b = makeNode(BooleanTest);
-- TODO b->arg = (Expr *) $1;
-- TODO b->booltesttype = IS_NOT_UNKNOWN;
-- TODO b->location = @2;
-- TODO $$ = (Node *)b;
-- TODO }
    | a_expr IS DISTINCT FROM a_expr %prec IS { BinOp IsDistinctFrom $1 $5 }
    | a_expr IS NOT DISTINCT FROM a_expr %prec IS { BinOp IsNotDistinctFrom $1 $6 }
-- TODO | a_expr IS OF '(' type_list ')'			%prec IS
-- TODO {
-- TODO $$ = (Node *) makeSimpleA_Expr(AEXPR_OF, "=", $1, (Node *) $5, @2);
-- TODO }
-- TODO | a_expr IS NOT OF '(' type_list ')'		%prec IS
-- TODO {
-- TODO $$ = (Node *) makeSimpleA_Expr(AEXPR_OF, "<>", $1, (Node *) $6, @2);
-- TODO }
-- TODO | a_expr BETWEEN opt_asymmetric b_expr AND a_expr		%prec BETWEEN
-- TODO {
-- TODO $$ = (Node *) makeSimpleA_Expr(AEXPR_BETWEEN,
-- TODO "BETWEEN",
-- TODO $1,
-- TODO (Node *) list_make2($4, $6),
-- TODO @2);
-- TODO }
-- TODO | a_expr NOT BETWEEN opt_asymmetric b_expr AND a_expr %prec NOT
-- TODO {
-- TODO $$ = (Node *) makeSimpleA_Expr(AEXPR_NOT_BETWEEN,
-- TODO "NOT BETWEEN",
-- TODO $1,
-- TODO (Node *) list_make2($5, $7),
-- TODO @2);
-- TODO }
-- TODO | a_expr BETWEEN SYMMETRIC b_expr AND a_expr			%prec BETWEEN
-- TODO {
-- TODO $$ = (Node *) makeSimpleA_Expr(AEXPR_BETWEEN_SYM,
-- TODO "BETWEEN SYMMETRIC",
-- TODO $1,
-- TODO (Node *) list_make2($4, $6),
-- TODO @2);
-- TODO }
-- TODO | a_expr NOT BETWEEN SYMMETRIC b_expr AND a_expr		%prec NOT
-- TODO {
-- TODO $$ = (Node *) makeSimpleA_Expr(AEXPR_NOT_BETWEEN_SYM,
-- TODO "NOT BETWEEN SYMMETRIC",
-- TODO $1,
-- TODO (Node *) list_make2($5, $7),
-- TODO @2);
-- TODO }
-- TODO | a_expr IN_P in_expr
-- TODO {
-- TODO /* in_expr returns a SubLink or a list of a_exprs */
-- TODO if (IsA($3, SubLink))
-- TODO {
-- TODO /* generate foo = ANY (subquery) */
-- TODO SubLink *n = (SubLink *) $3;
-- TODO n->subLinkType = ANY_SUBLINK;
-- TODO n->subLinkId = 0;
-- TODO n->testexpr = $1;
-- TODO n->operName = NIL;		/* show it's IN not = ANY */
-- TODO n->location = @2;
-- TODO $$ = (Node *)n;
-- TODO }
-- TODO else
-- TODO {
-- TODO /* generate scalar IN expression */
-- TODO $$ = (Node *) makeSimpleA_Expr(AEXPR_IN, "=", $1, $3, @2);
-- TODO }
-- TODO }
-- TODO | a_expr NOT IN_P in_expr						%prec NOT
-- TODO {
-- TODO /* in_expr returns a SubLink or a list of a_exprs */
-- TODO if (IsA($4, SubLink))
-- TODO {
-- TODO /* generate NOT (foo = ANY (subquery)) */
-- TODO /* Make an = ANY node */
-- TODO SubLink *n = (SubLink *) $4;
-- TODO n->subLinkType = ANY_SUBLINK;
-- TODO n->subLinkId = 0;
-- TODO n->testexpr = $1;
-- TODO n->operName = NIL;		/* show it's IN not = ANY */
-- TODO n->location = @2;
-- TODO /* Stick a NOT on top; must have same parse location */
-- TODO $$ = makeNotExpr((Node *) n, @2);
-- TODO }
-- TODO else
-- TODO {
-- TODO /* generate scalar NOT IN expression */
-- TODO $$ = (Node *) makeSimpleA_Expr(AEXPR_IN, "<>", $1, $4, @2);
-- TODO }
-- TODO }
-- TODO | a_expr subquery_Op sub_type select_with_parens	%prec Op
-- TODO {
-- TODO SubLink *n = makeNode(SubLink);
-- TODO n->subLinkType = $3;
-- TODO n->subLinkId = 0;
-- TODO n->testexpr = $1;
-- TODO n->operName = $2;
-- TODO n->subselect = $4;
-- TODO n->location = @2;
-- TODO $$ = (Node *)n;
-- TODO }
-- TODO | a_expr subquery_Op sub_type '(' a_expr ')'		%prec Op
-- TODO {
-- TODO if ($3 == ANY_SUBLINK)
-- TODO $$ = (Node *) makeA_Expr(AEXPR_OP_ANY, $2, $1, $5, @2);
-- TODO else
-- TODO $$ = (Node *) makeA_Expr(AEXPR_OP_ALL, $2, $1, $5, @2);
-- TODO }
-- TODO | UNIQUE select_with_parens
-- TODO {
-- TODO /* Not sure how to get rid of the parentheses
-- TODO * but there are lots of shift/reduce errors without them.
-- TODO *
-- TODO * Should be able to implement this by plopping the entire
-- TODO * select into a node, then transforming the target expressions
-- TODO * from whatever they are into count(*), and testing the
-- TODO * entire result equal to one.
-- TODO * But, will probably implement a separate node in the executor.
-- TODO */
-- TODO ereport(ERROR,
-- TODO (errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
-- TODO errmsg("UNIQUE predicate is not yet implemented"),
-- TODO parser_errposition(@1)));
-- TODO }
-- TODO | a_expr IS DOCUMENT_P					%prec IS
-- TODO {
-- TODO $$ = makeXmlExpr(IS_DOCUMENT, NULL, NIL,
-- TODO list_make1($1), @2);
-- TODO }
-- TODO | a_expr IS NOT DOCUMENT_P				%prec IS
-- TODO {
-- TODO $$ = makeNotExpr(makeXmlExpr(IS_DOCUMENT, NULL, NIL,
-- TODO list_make1($1), @2),
-- TODO @2);
-- TODO }
-- TODO | DEFAULT
-- TODO {
-- TODO /*
-- TODO * The SQL spec only allows DEFAULT in "contextually typed
-- TODO * expressions", but for us, it's easier to allow it in
-- TODO * any a_expr and then throw error during parse analysis
-- TODO * if it's in an inappropriate context.  This way also
-- TODO * lets us say something smarter than "syntax error".
-- TODO */
-- TODO SetToDefault *n = makeNode(SetToDefault);
-- TODO /* parse analysis will fill in the rest */
-- TODO n->location = @1;
-- TODO $$ = (Node *)n;
-- TODO }
-- TODO ;

-- * Restricted expressions
-- *
-- * b_expr is a subset of the complete expression syntax defined by a_expr.
-- *
-- * Presently, AND, NOT, IS, and IN are the a_expr keywords that would
-- * cause trouble in the places where b_expr is used.  For simplicity, we
-- * just eliminate all the boolean-keyword-operator productions from b_expr.
b_expr :: { Expr }
    : c_expr { $1 }
-- TODO | b_expr TYPECAST Typename
-- TODO 				{ $$ = makeTypeCast($1, $3, @2); }
    | '+' b_expr					%prec UMINUS { $2 } -- TODO keep + for round-trip?
-- TODO 				{ $$ = (Node *) makeSimpleA_Expr(AEXPR_OP, "+", NULL, $2, @1); }
    | '-' b_expr					%prec UMINUS { Unary Negate $2 }
    | b_expr '+' b_expr { BinOp Add $1 $3 }
    | b_expr '-' b_expr { BinOp Sub $1 $3 }
    | b_expr '*' b_expr { BinOp Mul $1 $3 }
    | b_expr '/' b_expr { BinOp Div $1 $3 }
    | b_expr '%' b_expr { BinOp Mod $1 $3 }
    | b_expr '^' b_expr { BinOp Exponent $1 $3 }
    | b_expr '<' b_expr { BinOp LT $1 $3 }
    | b_expr '>' b_expr { BinOp GT $1 $3 }
    | b_expr '=' b_expr { BinOp Eq $1 $3 }
    | b_expr '<=' b_expr { BinOp LTE $1 $3 }
    | b_expr '>=' b_expr { BinOp GTE $1 $3 }
    | b_expr '!=' b_expr { BinOp NEq $1 $3 }
    | b_expr qual_Op b_expr				%prec Op { BinOp $2 $1 $3 }
-- FIXME exclude user-defined operators, or give up on Syntax allowing only correct operator arity?
-- TODO 			| qual_Op b_expr					%prec Op
-- TODO 				{ $$ = (Node *) makeA_Expr(AEXPR_OP, $1, NULL, $2, @1); }
-- TODO 			| b_expr qual_Op					%prec POSTFIXOP
-- TODO 				{ $$ = (Node *) makeA_Expr(AEXPR_OP, $2, $1, NULL, @2); }
    | b_expr IS DISTINCT FROM b_expr		%prec IS { BinOp IsDistinctFrom $1 $5 }
    | b_expr IS NOT DISTINCT FROM b_expr	%prec IS { BinOp IsNotDistinctFrom $1 $6 }
-- TODO 			| b_expr IS OF '(' type_list ')'		%prec IS
-- TODO 				{
-- TODO 					$$ = (Node *) makeSimpleA_Expr(AEXPR_OF, "=", $1, (Node *) $5, @2);
-- TODO 				}
-- TODO 			| b_expr IS NOT OF '(' type_list ')'	%prec IS
-- TODO 				{
-- TODO 					$$ = (Node *) makeSimpleA_Expr(AEXPR_OF, "<>", $1, (Node *) $6, @2);
-- TODO 				}
-- TODO 			| b_expr IS DOCUMENT_P					%prec IS
-- TODO 				{
-- TODO 					$$ = makeXmlExpr(IS_DOCUMENT, NULL, NIL,
-- TODO 									 list_make1($1), @2);
-- TODO 				}
-- TODO 			| b_expr IS NOT DOCUMENT_P				%prec IS
-- TODO 				{
-- TODO 					$$ = makeNotExpr(makeXmlExpr(IS_DOCUMENT, NULL, NIL,
-- TODO 												 list_make1($1), @2),
-- TODO 									 @2);
-- TODO 				}

-- * Productions that can be used in both a_expr and b_expr.
-- *
-- * Note: productions that refer recursively to a_expr or b_expr mostly
-- * cannot appear here.	However, it's OK to refer to a_exprs that occur
-- * inside parentheses, such as function arguments; that cannot introduce
-- * ambiguity to the b_expr syntax.
c_expr :: { Expr }
    : columnref { $1 }
    | AexprConst { Lit $1 }
    -- TODO check_indirection
    | PARAM opt_indirection
        { case $2 of
          Nothing -> NumberedParam $1
          Just ne -> Indirection (NumberedParam $1) ne }
    | HASKELL_PARAM { HaskellParam $1 }
    | '(' a_expr ')' opt_indirection
      { case $4 of
        Nothing -> $2
        Just ne -> Indirection $2 ne }
    -- gram.y optionally warns about operator precedence
    | case_expr { Cas $1 }
    | func_expr { Fun $1 }
    | select_with_parens			%prec UMINUS { SelectExpr $1 }
    | select_with_parens indirection { Indirection (SelectExpr $1) $2 }
-- * Because the select_with_parens nonterminal is designed
-- * to "eat" as many levels of parens as possible, the
-- * '(' a_expr ')' opt_indirection production above will
-- * fail to match a sub-SELECT with indirection decoration;
-- * the sub-SELECT won't be regarded as an a_expr as long
-- * as there are parens around it.  To support applying
-- * subscripting or field selection to a sub-SELECT result,
-- * we need this redundant-looking production.
-- TODO 			| EXISTS select_with_parens
-- TODO 				{
-- TODO 					SubLink *n = makeNode(SubLink);
-- TODO 					n->subLinkType = EXISTS_SUBLINK;
-- TODO 					n->subLinkId = 0;
-- TODO 					n->testexpr = NULL;
-- TODO 					n->operName = NIL;
-- TODO 					n->subselect = $2;
-- TODO 					n->location = @1;
-- TODO 					$$ = (Node *)n;
-- TODO 				}
-- TODO 			| ARRAY select_with_parens
-- TODO 				{
-- TODO 					SubLink *n = makeNode(SubLink);
-- TODO 					n->subLinkType = ARRAY_SUBLINK;
-- TODO 					n->subLinkId = 0;
-- TODO 					n->testexpr = NULL;
-- TODO 					n->operName = NIL;
-- TODO 					n->subselect = $2;
-- TODO 					n->location = @1;
-- TODO 					$$ = (Node *)n;
-- TODO 				}
-- TODO 			| ARRAY array_expr
-- TODO 				{
-- TODO 					A_ArrayExpr *n = castNode(A_ArrayExpr, $2);
-- TODO 					/* point outermost A_ArrayExpr to the ARRAY keyword */
-- TODO 					n->location = @1;
-- TODO 					$$ = (Node *)n;
-- TODO 				}
-- TODO 			| explicit_row
-- TODO 				{
-- TODO 					RowExpr *r = makeNode(RowExpr);
-- TODO 					r->args = $1;
-- TODO 					r->row_typeid = InvalidOid;	/* not analyzed yet */
-- TODO 					r->colnames = NIL;	/* to be filled in during analysis */
-- TODO 					r->row_format = COERCE_EXPLICIT_CALL; /* abuse */
-- TODO 					r->location = @1;
-- TODO 					$$ = (Node *)r;
-- TODO 				}
-- TODO 			| implicit_row
-- TODO 				{
-- TODO 					RowExpr *r = makeNode(RowExpr);
-- TODO 					r->args = $1;
-- TODO 					r->row_typeid = InvalidOid;	/* not analyzed yet */
-- TODO 					r->colnames = NIL;	/* to be filled in during analysis */
-- TODO 					r->row_format = COERCE_IMPLICIT_CAST; /* abuse */
-- TODO 					r->location = @1;
-- TODO 					$$ = (Node *)r;
-- TODO 				}
-- TODO 			| GROUPING '(' expr_list ')'
-- TODO 			  {
-- TODO 				  GroupingFunc *g = makeNode(GroupingFunc);
-- TODO 				  g->args = $3;
-- TODO 				  g->location = @1;
-- TODO 				  $$ = (Node *)g;
-- TODO 			  }
-- TODO 		;

func_application :: { FunctionApplication }
    : func_name '(' ')' { fapp $1 NoArgs }
	  | func_name '(' func_arg_list opt_sort_clause ')'
        { fapp $1 (Args (argsList $3)) `setSortBy` $4 }
-- TODO     | func_name '(' VARIADIC func_arg_expr opt_sort_clause ')'
-- TODO 				{
-- TODO 					FuncCall *n = makeFuncCall($1, list_make1($4), @1);
-- TODO 					n->func_variadic = true;
-- TODO n->agg_order = $5;
-- TODO 					$$ = (Node *)n;
-- TODO }
-- TODO 			| func_name '(' func_arg_list ',' VARIADIC func_arg_expr opt_sort_clause ')'
-- TODO 				{
-- TODO 					FuncCall *n = makeFuncCall($1, lappend($3, $6), @1);
-- TODO 					n->func_variadic = true;
-- TODO n->agg_order = $7;
-- TODO 					$$ = (Node *)n;
-- TODO 				}
    | func_name '(' ALL func_arg_list opt_sort_clause ')'
        { fapp $1 (Args (argsList $4)) `setSortBy` $5 }
        -- * Ideally we'd mark the FuncCall node to indicate
        -- * "must be an aggregate", but there's no provision
        -- * for that in FuncCall at the moment.
        -- *
    | func_name '(' DISTINCT func_arg_list opt_sort_clause ')'
        { fapp $1 (Args (ArgsList $4 $5 True))}
    | func_name '(' '*' ')' { (fapp $1 StarArg) { arguments = StarArg } }

--  * func_expr and its cousin func_expr_windowless are split out from c_expr just
--  * so that we have classifications for "everything that is a function call or
--  * looks like one".  This isn't very important, but it saves us having to
--  * document which variants are legal in places like "FROM function()" or the
--  * backwards-compatible functional-index syntax for CREATE INDEX.
--  * (Note that many of the special SQL functions wouldn't actually make any
--  * sense as functional index entries, but we ignore that consideration here.)

func_expr :: { FunctionApplication }
    : func_application within_group_clause filter_clause over_clause
-- We disallow some cases that the PostgreSQL server disallows,
-- even though our representation doesn't have exactly the same
-- constraints.  Upstream comment:
-- * The order clause for WITHIN GROUP and the one for
-- * plain-aggregate ORDER BY share a field, so we have to
-- * check here that at most one is present.  We also check
-- * for DISTINCT and VARIADIC here to give a better error
-- * location.  Other consistency checks are deferred to
-- * parse analysis.
{% do
    result <- case ($1, $2) of
        (_, []) -> return $1
        (FApp{arguments}, _) -> do
            case arguments of
              Args ArgsList {distinct, sortBy} -> do
                when (not (null sortBy)) (fail "cannot use multiple ORDER BY clauses with WITHIN GROUP")
                when distinct (fail "cannot use DISTINCT with WITHIN GROUP")
              _ -> pure ()
            return $1 { withinGroup = $2 }
    return result { filterClause = $3 , over = $4 }
}
-- TODO 						if (n->func_variadic)
-- TODO 							ereport(ERROR,
-- TODO 									(errcode(ERRCODE_SYNTAX_ERROR),
-- TODO 									 errmsg("cannot use VARIADIC with WITHIN GROUP"),
-- TODO 									 parser_errposition(@2)));
-- TODO 						n->agg_order = $2;
-- TODO 						n->agg_within_group = true;
-- TODO 					}
    | func_expr_common_subexpr { $1 }

--  * as func_expr but does not accept window functions directly
--  * (but they can still be contained in arguments for functions etc).
--  * use this when window expressions are not allowed, where needed to
--  * disambiguate the grammar (e.g. in CREATE INDEX).

func_expr_windowless :: { FunctionApplication }
    : func_application { $1 }
    | func_expr_common_subexpr { $1 }

-- * Special expressions that are considered to be functions.

func_expr_common_subexpr :: { FunctionApplication }
    : COLLATION FOR '(' a_expr ')' { fapp1 "pg_collation_for" [$4] }
    | CURRENT_DATE { fapp1 "CURRENT_DATE" [] }
    | CURRENT_TIME { fapp1 "CURRENT_TIME" [] }
    | CURRENT_TIME '(' Iconst ')' { fapp1 "CURRENT_TIME" [Lit (I $3)] }
    | CURRENT_TIMESTAMP { fapp1 "CURRENT_TIMESTAMP" [] }
    | CURRENT_TIMESTAMP '(' Iconst ')' { fapp1 "CURRENT_TIMESTAMP" [Lit (I $3)] }
    | LOCALTIME { fapp1 "LOCALTIME" [] }
    | LOCALTIME '(' Iconst ')' { fapp1 "LOCALTIME" [Lit (I $3)] }
    | LOCALTIMESTAMP { fapp1 "LOCALTIMESTAMP" [] }
    | LOCALTIMESTAMP '(' Iconst ')' { fapp1 "LOCALTIMESTAMP" [Lit (I $3)] }
    | CURRENT_ROLE { fapp1 "CURRENT_ROLE" [] }
    | CURRENT_USER { fapp1 "CURRENT_USER" [] }
    | SESSION_USER { fapp1 "SESSION_USER" [] }
    | USER { fapp1 "USER" [] }
    | CURRENT_CATALOG { fapp1 "CURRENT_CATALOG" [] }
    | CURRENT_SCHEMA { fapp1 "CURRENT_SCHEMA" [] }
-- TODO | CAST '(' a_expr AS Typename ')' { $$ = makeTypeCast($3, $5, @1); }
-- TODO | EXTRACT '(' extract_list ')' { fapp1 "date_part" [$3] }
-- TODO 			| OVERLAY '(' overlay_list ')'
-- TODO 				{
-- TODO 					/* overlay(A PLACING B FROM C FOR D) is converted to
-- TODO 					 * overlay(A, B, C, D)
-- TODO 					 * overlay(A PLACING B FROM C) is converted to
-- TODO 					 * overlay(A, B, C)
-- TODO 					 */
-- TODO 					$$ = (Node *) makeFuncCall(SystemFuncName("overlay"), $3, @1);
-- TODO 				}
-- TODO 			| POSITION '(' position_list ')'
-- TODO 				{
-- TODO 					/* position(A in B) is converted to position(B, A) */
-- TODO 					$$ = (Node *) makeFuncCall(SystemFuncName("position"), $3, @1);
-- TODO 				}
-- TODO 			| SUBSTRING '(' substr_list ')'
-- TODO 				{
-- TODO 					/* substring(A from B for C) is converted to
-- TODO 					 * substring(A, B, C) - thomas 2000-11-28
-- TODO 					 */
-- TODO 					$$ = (Node *) makeFuncCall(SystemFuncName("substring"), $3, @1);
-- TODO 				}
-- TODO 			| TREAT '(' a_expr AS Typename ')'
-- TODO 				{
-- TODO 					/* TREAT(expr AS target) converts expr of a particular type to target,
-- TODO 					 * which is defined to be a subtype of the original expression.
-- TODO 					 * In SQL99, this is intended for use with structured UDTs,
-- TODO 					 * but let's make this a generally useful form allowing stronger
-- TODO 					 * coercions than are handled by implicit casting.
-- TODO 					 *
-- TODO 					 * Convert SystemTypeName() to SystemFuncName() even though
-- TODO 					 * at the moment they result in the same thing.
-- TODO 					 */
-- TODO 					$$ = (Node *) makeFuncCall(SystemFuncName(((Value *)llast($5->names))->val.str),
-- TODO 												list_make1($3),
-- TODO 												@1);
-- TODO 				}
-- TODO 			| TRIM '(' BOTH trim_list ')'
-- TODO 				{
-- TODO 					/* various trim expressions are defined in SQL
-- TODO 					 * - thomas 1997-07-19
-- TODO 					 */
-- TODO 					$$ = (Node *) makeFuncCall(SystemFuncName("btrim"), $4, @1);
-- TODO 				}
-- TODO 			| TRIM '(' LEADING trim_list ')'
-- TODO 				{
-- TODO 					$$ = (Node *) makeFuncCall(SystemFuncName("ltrim"), $4, @1);
-- TODO 				}
-- TODO 			| TRIM '(' TRAILING trim_list ')'
-- TODO 				{
-- TODO 					$$ = (Node *) makeFuncCall(SystemFuncName("rtrim"), $4, @1);
-- TODO 				}
-- TODO 			| TRIM '(' trim_list ')'
-- TODO 				{
-- TODO 					$$ = (Node *) makeFuncCall(SystemFuncName("btrim"), $3, @1);
-- TODO 				}
-- TODO 			| NULLIF '(' a_expr ',' a_expr ')'
-- TODO 				{
-- TODO 					$$ = (Node *) makeSimpleA_Expr(AEXPR_NULLIF, "=", $3, $5, @1);
-- TODO 				}
    | COALESCE '(' expr_list ')' { fapp1 "coalesce" $3 }
    | GREATEST '(' expr_list ')' { fapp1 "greatest" $3 }
    | LEAST '(' expr_list ')' { fapp1 "least" $3 }
-- TODO 			| XMLCONCAT '(' expr_list ')'
-- TODO 				{
-- TODO 					$$ = makeXmlExpr(IS_XMLCONCAT, NULL, NIL, $3, @1);
-- TODO 				}
-- TODO 			| XMLELEMENT '(' NAME_P ColLabel ')'
-- TODO 				{
-- TODO 					$$ = makeXmlExpr(IS_XMLELEMENT, $4, NIL, NIL, @1);
-- TODO 				}
-- TODO 			| XMLELEMENT '(' NAME_P ColLabel ',' xml_attributes ')'
-- TODO 				{
-- TODO 					$$ = makeXmlExpr(IS_XMLELEMENT, $4, $6, NIL, @1);
-- TODO 				}
-- TODO 			| XMLELEMENT '(' NAME_P ColLabel ',' expr_list ')'
-- TODO 				{
-- TODO 					$$ = makeXmlExpr(IS_XMLELEMENT, $4, NIL, $6, @1);
-- TODO 				}
-- TODO 			| XMLELEMENT '(' NAME_P ColLabel ',' xml_attributes ',' expr_list ')'
-- TODO 				{
-- TODO 					$$ = makeXmlExpr(IS_XMLELEMENT, $4, $6, $8, @1);
-- TODO 				}
-- TODO 			| XMLEXISTS '(' c_expr xmlexists_argument ')'
-- TODO 				{
-- TODO 					/* xmlexists(A PASSING [BY REF] B [BY REF]) is
-- TODO 					 * converted to xmlexists(A, B)*/
-- TODO 					$$ = (Node *) makeFuncCall(SystemFuncName("xmlexists"), list_make2($3, $4), @1);
-- TODO 				}
-- TODO 			| XMLFOREST '(' xml_attribute_list ')'
-- TODO 				{
-- TODO 					$$ = makeXmlExpr(IS_XMLFOREST, NULL, $3, NIL, @1);
-- TODO 				}
-- TODO 			| XMLPARSE '(' document_or_content a_expr xml_whitespace_option ')'
-- TODO 				{
-- TODO 					XmlExpr *x = (XmlExpr *)
-- TODO 						makeXmlExpr(IS_XMLPARSE, NULL, NIL,
-- TODO 									list_make2($4, makeBoolAConst($5, -1)),
-- TODO 									@1);
-- TODO 					x->xmloption = $3;
-- TODO 					$$ = (Node *)x;
-- TODO 				}
-- TODO 			| XMLPI '(' NAME_P ColLabel ')'
-- TODO 				{
-- TODO 					$$ = makeXmlExpr(IS_XMLPI, $4, NULL, NIL, @1);
-- TODO 				}
-- TODO 			| XMLPI '(' NAME_P ColLabel ',' a_expr ')'
-- TODO 				{
-- TODO 					$$ = makeXmlExpr(IS_XMLPI, $4, NULL, list_make1($6), @1);
-- TODO 				}
-- TODO 			| XMLROOT '(' a_expr ',' xml_root_version opt_xml_root_standalone ')'
-- TODO 				{
-- TODO 					$$ = makeXmlExpr(IS_XMLROOT, NULL, NIL,
-- TODO 									 list_make3($3, $5, $6), @1);
-- TODO 				}
-- TODO 			| XMLSERIALIZE '(' document_or_content a_expr AS SimpleTypename ')'
-- TODO 				{
-- TODO 					XmlSerialize *n = makeNode(XmlSerialize);
-- TODO 					n->xmloption = $3;
-- TODO 					n->expr = $4;
-- TODO 					n->typeName = $6;
-- TODO 					n->location = @1;
-- TODO 					$$ = (Node *)n;
-- TODO 				}
-- TODO 		;

-- TODO xml_root_version ::
-- TODO opt_xml_root_standalone ::
-- TODO xml_attributes ::
-- TODO xml_attribute_list ::
-- TODO xml_attribute_el ::
-- TODO document_or_content ::
-- TODO xml_whitespace_option ::
-- TODO xmlexists_argument ::
-- TODO xml_passing_mech ::

-- * Aggregate decoration clauses
within_group_clause :: { [SortBy] }
    : WITHIN GROUP_P '(' sort_clause ')' { $4 }
    | { [] }

filter_clause :: { Maybe Expr }
    : FILTER '(' WHERE a_expr ')' { Just $4 }
    | { Nothing }

-- * Window Definitions
window_clause
    : WINDOW window_definition_list { reverse $2 }
    | { [] }

window_definition_list : list(window_definition) { $1 }

window_definition :: { WindowDef }
    : ColId AS window_specification { WindowDef $1 $3 }

over_clause :: { Over }
: OVER window_specification { Window $2 }
| OVER ColId { WindowName $2 }
| { noWindow }

window_specification :: { WindowSpec }
: '(' opt_existing_window_name opt_partition_clause opt_sort_clause opt_frame_clause ')'
    { WindowSpec $2 $3 $4 }

-- * If we see PARTITION, RANGE, ROWS or GROUPS as the first token after the '('
-- * of a window_specification, we want the assumption to be that there is
-- * no existing_window_name; but those keywords are unreserved and so could
-- * be ColIds.  We fix this by making them have the same precedence as IDENT
-- * and giving the empty production here a slightly higher precedence, so
-- * that the shift/reduce conflict is resolved in favor of reducing the rule.
-- * These keywords are thus precluded from being an existing_window_name but
-- * are not reserved for any other purpose.
opt_existing_window_name :: { Maybe Text }
    : ColId						{ Just $1 }
    | 	%prec Op		{ Nothing }

opt_partition_clause :: { [Expr] }
    : PARTITION BY expr_list		{ $3 }
    | { [] }

-- * For frame clauses, we return a WindowDef, but only some fields are used:
-- * frameOptions, startOffset, and endOffset.
-- FIXME What is this, how do I want to handle the bitflags?
opt_frame_clause : { () }
-- TODO 			RANGE frame_extent opt_window_exclusion_clause
-- TODO 				{
-- TODO 					WindowDef *n = $2;
-- TODO 					n->frameOptions |= FRAMEOPTION_NONDEFAULT | FRAMEOPTION_RANGE;
-- TODO 					n->frameOptions |= $3;
-- TODO 					$$ = n;
-- TODO 				}
-- TODO 			| ROWS frame_extent opt_window_exclusion_clause
-- TODO 				{
-- TODO 					WindowDef *n = $2;
-- TODO 					n->frameOptions |= FRAMEOPTION_NONDEFAULT | FRAMEOPTION_ROWS;
-- TODO 					n->frameOptions |= $3;
-- TODO 					$$ = n;
-- TODO 				}
-- TODO 			| GROUPS frame_extent opt_window_exclusion_clause
-- TODO 				{
-- TODO 					WindowDef *n = $2;
-- TODO 					n->frameOptions |= FRAMEOPTION_NONDEFAULT | FRAMEOPTION_GROUPS;
-- TODO 					n->frameOptions |= $3;
-- TODO 					$$ = n;
-- TODO 				}
-- TODO 			| /*EMPTY*/
-- TODO 				{
-- TODO 					WindowDef *n = makeNode(WindowDef);
-- TODO 					n->frameOptions = FRAMEOPTION_DEFAULTS;
-- TODO 					n->startOffset = NULL;
-- TODO 					n->endOffset = NULL;
-- TODO 					$$ = n;
-- TODO 				}
-- TODO 		;

-- TODO frame_extent ::
-- TODO frame_bound ::
-- TODO opt_window_exclusion_clause ::
-- TODO row ::
-- TODO explicit_row ::
-- TODO implicit_row ::
-- TODO sub_type ::

all_Op : MathOp { $1 }
-- We don't (yet?) support user-defined operators

MathOp :: { BinOp }
    : '+'									{ Add }
    | '-'									{ Sub }
    | '*'									{ Mul }
    | '/'									{ Div }
    | '%'									{ Mod }
    | '^'									{ Exponent }
    | '<'									{ LT }
    | '>'									{ GT }
    | '='									{ Eq }
    | '<='							{ LTE }
    | '>='						{ GTE }
    | '!='							{ NEq }

qual_Op
    -- We don't (yet?) support user-defined operators
    -- :	Op { $1 }
    : OPERATOR '(' any_operator ')' { $3 }

qual_all_Op
    : all_Op { $1 }
    | OPERATOR '(' any_operator ')' { $3 }

-- TODO subquery_Op ::

expr_list : list(a_expr) { reverse $1 }

-- * function arguments can have names
func_arg_list : list(func_arg_expr) { NE.fromList (reverse $1) }

func_arg_expr :: { Argument }
    :  a_expr { E $1 }
	  | param_name COLON_EQUALS a_expr { Named $1 $3 }
	  | param_name EQUALS_GREATER a_expr { Named $1 $3 }

-- TODO type_list::
-- TODO array_expr ::
-- TODO array_expr_list ::
-- TODO extract_list ::
-- TODO extract_arg ::
-- TODO overlay_list ::
-- TODO overlay_placing ::
-- TODO position_list ::
-- TODO substr_list ::
-- TODO substr_from ::
-- TODO substr_for ::
-- TODO trim_list ::
-- TODO in_expr ::

-- * Define SQL-style CASE clause.
-- * - Full specification
-- *	CASE WHEN a = b THEN c ... ELSE d END
-- * - Implicit argument
-- *	CASE a WHEN b THEN c ... ELSE d END
case_expr :: { Case }
    : CASE case_arg when_clause_list case_default END_P
        { Case { implicitArg = $2, whenClause = reverse $3, elseClause = $4 } }

when_clause_list :: { [(Expr, Expr)] }
    : when_clause { [$1] }
    | when_clause_list when_clause { $2 : $1 }

when_clause :: { (Expr, Expr) }
    : WHEN a_expr THEN a_expr { ($2, $4) }

case_default :: { Maybe Expr }
    : ELSE a_expr { Just $2 }
    | { Nothing }

case_arg :: { Maybe Expr }
    : a_expr { Just $1 }
    | { Nothing }


-- * Ideally param_name should be ColId, but that causes too many conflicts.
param_name :: { Text }
           :	type_function_name { $1 }

-- FIXME handwritten
Insert : INSERT INTO relation_expr '(' name_list ')' VALUES '(' expr_list ')'
       { Insert { table = $3, columns = NE.fromList (reverse $5), values = NE.fromList $9 } }

Update :: { Update }
    : UPDATE relation_expr SET SettingList WHERE a_expr { Update { table = $2, settings = NE.fromList (reverse $4), conditions = Just $6 } }
    | UPDATE relation_expr SET SettingList { Update { table = $2, settings = NE.fromList (reverse $4), conditions = Nothing } }

{- These lists are non-empty by construction, but not by type. List head is the right-most element. -}

list(el)
    : el { [$1] }
    | list(el) ',' el { $3 : $1 }

SettingList : list(Setting) { $1 }


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

Setting :: { Setting }
    : ColId '=' a_expr { Setting $1 $3 }

Null
        : IS NULL_P { IsNull }
        | ISNULL { IsNull }
        | IS NOT NULL_P { NotNull }
        | NOTNULL { NotNull }

columnref :: { Expr }
    : ColId { CRef $1 }
    | ColId indirection { Indirection (CRef $1) $2 }

indirection_el :: { Text } -- TODO bigger type
    : '.' attr_name { $2 }
-- TODO 				{
-- TODO 					$$ = (Node *) makeString($2);
-- TODO 				}
-- TODO 			| '.' '*'
-- TODO 				{
-- TODO 					$$ = (Node *) makeNode(A_Star);
-- TODO 				}
-- TODO 			| '[' a_expr ']'
-- TODO 				{
-- TODO 					A_Indices *ai = makeNode(A_Indices);
-- TODO 					ai->is_slice = false;
-- TODO 					ai->lidx = NULL;
-- TODO 					ai->uidx = $2;
-- TODO 					$$ = (Node *) ai;
-- TODO 				}
-- TODO 			| '[' opt_slice_bound ':' opt_slice_bound ']'
-- TODO 				{
-- TODO 					A_Indices *ai = makeNode(A_Indices);
-- TODO 					ai->is_slice = true;
-- TODO 					ai->lidx = $2;
-- TODO 					ai->uidx = $4;
-- TODO 					$$ = (Node *) ai;
-- TODO 				}
-- TODO 		;

-- TODO opt_slice_bound:
-- TODO 			a_expr									{ $$ = $1; }
-- TODO 			| /*EMPTY*/								{ $$ = NULL; }
-- TODO 		;

indirection :: { NonEmpty Text }
  : rev_indirection { NE.reverse $1 }

rev_indirection :: { NonEmpty Text }
  : indirection_el { $1 :| [] }
  | rev_indirection indirection_el { NE.cons $2 $1 }

opt_indirection :: { Maybe (NonEmpty Text) }
			: { Nothing }
			| indirection { Just $1 }

-- TODO opt_asymmetric: ASYMMETRIC
-- TODO 			| /*EMPTY*/
-- TODO 		;

-- *	target list for SELECT

opt_target_list :: { [ResTarget] }
    : target_list { $1 }
    | { [] }

target_list : list(target_el) { reverse $1 }

target_el :: { ResTarget }
    : a_expr AS ColLabel { Column $1 (Just $3) }
    | a_expr ColLabel { Column $1 (Just $2) }
    | a_expr { Column $1 Nothing }
    | '*' { Star }

 -- *	Names and constants
qualified_name_list : list(qualified_name) { $1 }

--  * The production for a qualified relation name has to exactly match the
--  * production for a qualified func_name, because in a FROM clause we cannot
--  * tell which we are parsing until we see what comes after it ('(' for a
--  * func_name, something else for a relation). Therefore we allow 'indirection'
--  * which may contain subscripts, and reject that case in the C code.

qualified_name :: { Text }
    : ColId { $1 }
-- -- TODO 			| ColId indirection
-- TODO 				{
-- TODO 					check_qualified_name($2, yyscanner);
-- TODO 					$$ = makeRangeVar(NULL, NULL, @1);
-- TODO 					switch (list_length($2))
-- TODO 					{
-- TODO 						case 1:
-- TODO 							$$->catalogname = NULL;
-- TODO 							$$->schemaname = $1;
-- TODO 							$$->relname = strVal(linitial($2));
-- TODO 							break;
-- TODO 						case 2:
-- TODO 							$$->catalogname = $1;
-- TODO 							$$->schemaname = strVal(linitial($2));
-- TODO 							$$->relname = strVal(lsecond($2));
-- TODO 							break;
-- TODO 						default:
-- TODO 							ereport(ERROR,
-- TODO 									(errcode(ERRCODE_SYNTAX_ERROR),
-- TODO 									 errmsg("improper qualified name (too many dotted names): %s",
-- TODO 											NameListToString(lcons(makeString($1), $2))),
-- TODO 									 parser_errposition(@1)));
-- TODO 							break;
-- TODO 					}
-- TODO 				}

name_list : list(name) { $1 }

opt_name_list
    : '(' name_list ')' { $2 }
    | { [] }

name : ColId { $1 }

database_name : ColId { $1 }

access_method : ColId { $1 }

attr_name : ColLabel { $1 }

index_name : ColId { $1 }

file_name :	Sconst { $1 }

-- * The production for a qualified func_name has to exactly match the
-- * production for a qualified columnref, because we cannot tell which we
-- * are parsing until we see what comes after it ('(' or Sconst for a func_name,
-- * anything else for a columnref).  Therefore we allow 'indirection' which
-- * may contain subscripts, and reject that case in the C code.  (If we
-- * ever implement SQL99-like methods, such syntax may actually become legal!)
func_name :: { (Text, [Text]) }
    :	type_function_name { ($1, []) }
	  | ColId indirection { ($1, NE.toList $2) }

-- * Constants

AexprConst :: { Literal }
    : Iconst { I $1 }
    | Fconst { F $1 }
    | Sconst { T $1 }
-- TODO 			| BCONST
-- TODO 				{
-- TODO 					$$ = makeBitStringConst($1, @1);
-- TODO 				}
-- TODO 			| XCONST
-- TODO 				{
-- TODO 					/* This is a bit constant per SQL99:
-- TODO 					 * Without Feature F511, "BIT data type",
-- TODO 					 * a <general literal> shall not be a
-- TODO 					 * <bit string literal> or a <hex string literal>.
-- TODO 					 */
-- TODO 					$$ = makeBitStringConst($1, @1);
-- TODO 				}
-- TODO 			| func_name Sconst
-- TODO 				{
-- TODO 					/* generic type 'literal' syntax */
-- TODO 					TypeName *t = makeTypeNameFromNameList($1);
-- TODO 					t->location = @1;
-- TODO 					$$ = makeStringConstCast($2, @2, t);
-- TODO 				}
-- TODO 			| func_name '(' func_arg_list opt_sort_clause ')' Sconst
-- TODO 				{
-- TODO 					/* generic syntax with a type modifier */
-- TODO 					TypeName *t = makeTypeNameFromNameList($1);
-- TODO 					ListCell *lc;
-- TODO
-- TODO 					/*
-- TODO 					 * We must use func_arg_list and opt_sort_clause in the
-- TODO 					 * production to avoid reduce/reduce conflicts, but we
-- TODO 					 * don't actually wish to allow NamedArgExpr in this
-- TODO 					 * context, nor ORDER BY.
-- TODO 					 */
-- TODO 					foreach(lc, $3)
-- TODO 					{
-- TODO 						NamedArgExpr *arg = (NamedArgExpr *) lfirst(lc);
-- TODO
-- TODO 						if (IsA(arg, NamedArgExpr))
-- TODO 							ereport(ERROR,
-- TODO 									(errcode(ERRCODE_SYNTAX_ERROR),
-- TODO 									 errmsg("type modifier cannot have parameter name"),
-- TODO 									 parser_errposition(arg->location)));
-- TODO 					}
-- TODO 					if ($4 != NIL)
-- TODO 							ereport(ERROR,
-- TODO 									(errcode(ERRCODE_SYNTAX_ERROR),
-- TODO 									 errmsg("type modifier cannot have ORDER BY"),
-- TODO 									 parser_errposition(@4)));
-- TODO
-- TODO 					t->typmods = $3;
-- TODO 					t->location = @1;
-- TODO 					$$ = makeStringConstCast($6, @6, t);
-- TODO 				}
-- TODO 			| ConstTypename Sconst
-- TODO 				{
-- TODO 					$$ = makeStringConstCast($2, @2, $1);
-- TODO 				}
-- TODO 			| ConstInterval Sconst opt_interval
-- TODO 				{
-- TODO 					TypeName *t = $1;
-- TODO 					t->typmods = $3;
-- TODO 					$$ = makeStringConstCast($2, @2, t);
-- TODO 				}
-- TODO 			| ConstInterval '(' Iconst ')' Sconst
-- TODO 				{
-- TODO 					TypeName *t = $1;
-- TODO 					t->typmods = list_make2(makeIntConst(INTERVAL_FULL_RANGE, -1),
-- TODO 											makeIntConst($3, @3));
-- TODO 					$$ = makeStringConstCast($5, @5, t);
-- TODO 				}
    | TRUE_P { B True }
    | FALSE_P { B False }
    | NULL_P { Null }

-- TODO Iconst : ICONST { $1 }
-- TODO rename STRING -> SCONST to match bison
-- TODO Sconst : SCONST { $1 }
Sconst : STRING { $1 }

-- TODO SignedIconst
-- TODO     : Iconst								{ $1 }
-- TODO     | '+' Iconst							{ + $2 }
-- TODO     | '-' Iconst							{ - $2 }
-- TODO

-- * Name classification hierarchy.
-- *
-- * IDENT is the lexeme returned by the lexer for identifiers that match
-- * no known keyword.  In most cases, we can accept certain keywords as
-- * names, not only IDENTs.	We prefer to accept as many such keywords
-- * as possible to minimize the impact of "reserved words" on programmers.
-- * So, we divide names into several possible classes.  The classification
-- * is chosen in part to make keywords acceptable as names wherever possible.

-- Column identifier --- names that can be column, table, etc names.
ColId :: { Text }
    :		IDENT									{ $1 }
    | unreserved_keyword					{ $1 }
		| col_name_keyword						{ $1 }

-- * Type/function identifier -- *- names that can be type or function names.
type_function_name :: { Text }
    :	IDENT							{ $1 }
    | unreserved_keyword					{ $1 }
    | type_func_name_keyword				{ $1 }

-- * Any not-fully-reserved word -- *- these names can be, eg, role names.
NonReservedWord  :: { Text }
     :	IDENT							{ $1 }
			| unreserved_keyword					{ $1 }
			| col_name_keyword						{ $1 }
			| type_func_name_keyword				{ $1 }

-- * Column label -- *- allowed labels in "AS" clauses.
-- * This presently includes *all* Postgres keywords.
ColLabel :: { Text }
    :	IDENT									{  $1 }
			| unreserved_keyword					{  $1 }
			| col_name_keyword						{  $1 }
			| type_func_name_keyword				{  $1 }
			| reserved_keyword						{  $1 }

-- * Keyword category lists.  Generally, every keyword present in
-- * the Postgres grammar should appear in exactly one of these lists.
-- *
-- * Put a new keyword into the first list that it can go into without causing
-- * shift or reduce conflicts.  The earlier lists define "less reserved"
-- * categories of keywords.
-- *
-- * Make sure that each keyword's category in kwlist.h matches where
-- * it is listed here.  (Someday we may be able to generate these lists and
-- * kwlist.h's table from a common master list.)

-- * "Unreserved" keywords --- available for use as any kind of name.
unreserved_keyword :: { Text }
    : ABORT_P { "abort" }
    | ABSOLUTE_P { "absolute" }
    | ACCESS { "access" }
    | ACTION { "action" }
    | ADD_P { "add" }
    | ADMIN { "admin" }
    | AFTER { "after" }
    | AGGREGATE { "aggregate" }
    | ALSO { "also" }
    | ALTER { "alter" }
    | ALWAYS { "always" }
    | ASSERTION { "assertion" }
    | ASSIGNMENT { "assignment" }
    | AT { "at" }
    | ATTACH { "attach" }
    | ATTRIBUTE { "attribute" }
    | BACKWARD { "backward" }
    | BEFORE { "before" }
    | BEGIN_P { "begin" }
    | BY { "by" }
    | CACHE { "cache" }
    | CALL { "call" }
    | CALLED { "called" }
    | CASCADE { "cascade" }
    | CASCADED { "cascaded" }
    | CATALOG_P { "catalog" }
    | CHAIN { "chain" }
    | CHARACTERISTICS { "characteristics" }
    | CHECKPOINT { "checkpoint" }
    | CLASS { "class" }
    | CLOSE { "close" }
    | CLUSTER { "cluster" }
    | COLUMNS { "columns" }
    | COMMENT { "comment" }
    | COMMENTS { "comments" }
    | COMMIT { "commit" }
    | COMMITTED { "committed" }
    | CONFIGURATION { "configuration" }
    | CONFLICT { "conflict" }
    | CONNECTION { "connection" }
    | CONSTRAINTS { "constraints" }
    | CONTENT_P { "content" }
    | CONTINUE_P { "continue" }
    | CONVERSION_P { "conversion" }
    | COPY { "copy" }
    | COST { "cost" }
    | CSV { "csv" }
    | CUBE { "cube" }
    | CURRENT_P { "current" }
    | CURSOR { "cursor" }
    | CYCLE { "cycle" }
    | DATA_P { "data" }
    | DATABASE { "database" }
    | DAY_P { "day" }
    | DEALLOCATE { "deallocate" }
    | DECLARE { "declare" }
    | DEFAULTS { "defaults" }
    | DEFERRED { "deferred" }
    | DEFINER { "definer" }
    | DELETE_P { "delete" }
    | DELIMITER { "delimiter" }
    | DELIMITERS { "delimiters" }
    | DEPENDS { "depends" }
    | DETACH { "detach" }
    | DICTIONARY { "dictionary" }
    | DISABLE_P { "disable" }
    | DISCARD { "discard" }
    | DOCUMENT_P { "document" }
    | DOMAIN_P { "domain" }
    | DOUBLE_P { "double" }
    | DROP { "drop" }
    | EACH { "each" }
    | ENABLE_P { "enable" }
    | ENCODING { "encoding" }
    | ENCRYPTED { "encrypted" }
    | ENUM_P { "enum" }
    | ESCAPE { "escape" }
    | EVENT { "event" }
    | EXCLUDE { "exclude" }
    | EXCLUDING { "excluding" }
    | EXCLUSIVE { "exclusive" }
    | EXECUTE { "execute" }
    | EXPLAIN { "explain" }
    | EXTENSION { "extension" }
    | EXTERNAL { "external" }
    | FAMILY { "family" }
    | FILTER { "filter" }
    | FIRST_P { "first" }
    | FOLLOWING { "following" }
    | FORCE { "force" }
    | FORWARD { "forward" }
    | FUNCTION { "function" }
    | FUNCTIONS { "functions" }
    | GENERATED { "generated" }
    | GLOBAL { "global" }
    | GRANTED { "granted" }
    | GROUPS { "groups" }
    | HANDLER { "handler" }
    | HEADER_P { "header" }
    | HOLD { "hold" }
    | HOUR_P { "hour" }
    | IDENTITY_P { "identity" }
    | IF_P { "if" }
    | IMMEDIATE { "immediate" }
    | IMMUTABLE { "immutable" }
    | IMPLICIT_P { "implicit" }
    | IMPORT_P { "import" }
    | INCLUDE { "include" }
    | INCLUDING { "including" }
    | INCREMENT { "increment" }
    | INDEX { "index" }
    | INDEXES { "indexes" }
    | INHERIT { "inherit" }
    | INHERITS { "inherits" }
    | INLINE_P { "inline" }
    | INPUT_P { "input" }
    | INSENSITIVE { "insensitive" }
    | INSERT { "insert" }
    | INSTEAD { "instead" }
    | INVOKER { "invoker" }
    | ISOLATION { "isolation" }
    | KEY { "key" }
    | LABEL { "label" }
    | LANGUAGE { "language" }
    | LARGE_P { "large" }
    | LAST { "last" }
    | LEAKPROOF { "leakproof" }
    | LEVEL { "level" }
    | LISTEN { "listen" }
    | LOAD { "load" }
    | LOCAL { "local" }
    | LOCATION { "location" }
    | LOCK_P { "lock" }
    | LOCKED { "locked" }
    | LOGGED { "logged" }
    | MAPPING { "mapping" }
    | MATCH { "match" }
    | MATERIALIZED { "materialized" }
    | MAXVALUE { "maxvalue" }
    | METHOD { "method" }
    | MINUTE_P { "minute" }
    | MINVALUE { "minvalue" }
    | MODE { "mode" }
    | MONTH_P { "month" }
    | MOVE { "move" }
    | NAME_P { "name" }
    | NAMES { "names" }
    | NEW { "new" }
    | NEXT { "next" }
    | NO { "no" }
    | NOTHING { "nothing" }
    | NOTIFY { "notify" }
    | NOWAIT { "nowait" }
    | NULLS_P { "nulls" }
    | OBJECT_P { "object" }
    | OF { "of" }
    | OFF { "off" }
    | OIDS { "oids" }
    | OLD { "old" }
    | OPERATOR { "operator" }
    | OPTION { "option" }
    | OPTIONS { "options" }
    | ORDINALITY { "ordinality" }
    | OTHERS { "others" }
    | OVER { "over" }
    | OVERRIDING { "overriding" }
    | OWNED { "owned" }
    | OWNER { "owner" }
    | PARALLEL { "parallel" }
    | PARSER { "parser" }
    | PARTIAL { "partial" }
    | PARTITION { "partition" }
    | PASSING { "passing" }
    | PASSWORD { "password" }
    | PLANS { "plans" }
    | POLICY { "policy" }
    | PRECEDING { "preceding" }
    | PREPARE { "prepare" }
    | PREPARED { "prepared" }
    | PRESERVE { "preserve" }
    | PRIOR { "prior" }
    | PRIVILEGES { "privileges" }
    | PROCEDURAL { "procedural" }
    | PROCEDURE { "procedure" }
    | PROCEDURES { "procedures" }
    | PROGRAM { "program" }
    | PUBLICATION { "publication" }
    | QUOTE { "quote" }
    | RANGE { "range" }
    | READ { "read" }
    | REASSIGN { "reassign" }
    | RECHECK { "recheck" }
    | RECURSIVE { "recursive" }
    | REF { "ref" }
    | REFERENCING { "referencing" }
    | REFRESH { "refresh" }
    | REINDEX { "reindex" }
    | RELATIVE_P { "relative" }
    | RELEASE { "release" }
    | RENAME { "rename" }
    | REPEATABLE { "repeatable" }
    | REPLACE { "replace" }
    | REPLICA { "replica" }
    | RESET { "reset" }
    | RESTART { "restart" }
    | RESTRICT { "restrict" }
    | RETURNS { "returns" }
    | REVOKE { "revoke" }
    | ROLE { "role" }
    | ROLLBACK { "rollback" }
    | ROLLUP { "rollup" }
    | ROUTINE { "routine" }
    | ROUTINES { "routines" }
    | ROWS { "rows" }
    | RULE { "rule" }
    | SAVEPOINT { "savepoint" }
    | SCHEMA { "schema" }
    | SCHEMAS { "schemas" }
    | SCROLL { "scroll" }
    | SEARCH { "search" }
    | SECOND_P { "second" }
    | SECURITY { "security" }
    | SEQUENCE { "sequence" }
    | SEQUENCES { "sequences" }
    | SERIALIZABLE { "serializable" }
    | SERVER { "server" }
    | SESSION { "session" }
    | SET { "set" }
    | SETS { "sets" }
    | SHARE { "share" }
    | SHOW { "show" }
    | SIMPLE { "simple" }
    | SKIP { "skip" }
    | SNAPSHOT { "snapshot" }
    | SQL_P { "sql" }
    | STABLE { "stable" }
    | STANDALONE_P { "standalone" }
    | START { "start" }
    | STATEMENT { "statement" }
    | STATISTICS { "statistics" }
    | STDIN { "stdin" }
    | STDOUT { "stdout" }
    | STORAGE { "storage" }
    | STORED { "stored" }
    | STRICT_P { "strict" }
    | STRIP_P { "strip" }
    | SUBSCRIPTION { "subscription" }
    | SUPPORT { "support" }
    | SYSID { "sysid" }
    | SYSTEM_P { "system" }
    | TABLES { "tables" }
    | TABLESPACE { "tablespace" }
    | TEMP { "temp" }
    | TEMPLATE { "template" }
    | TEMPORARY { "temporary" }
    | TEXT_P { "text" }
    | TIES { "ties" }
    | TRANSACTION { "transaction" }
    | TRANSFORM { "transform" }
    | TRIGGER { "trigger" }
    | TRUNCATE { "truncate" }
    | TRUSTED { "trusted" }
    | TYPE_P { "type" }
    | TYPES_P { "types" }
    | UNBOUNDED { "unbounded" }
    | UNCOMMITTED { "uncommitted" }
    | UNENCRYPTED { "unencrypted" }
    | UNKNOWN { "unknown" }
    | UNLISTEN { "unlisten" }
    | UNLOGGED { "unlogged" }
    | UNTIL { "until" }
    | UPDATE { "update" }
    | VACUUM { "vacuum" }
    | VALID { "valid" }
    | VALIDATE { "validate" }
    | VALIDATOR { "validator" }
    | VALUE_P { "value" }
    | VARYING { "varying" }
    | VERSION_P { "version" }
    | VIEW { "view" }
    | VIEWS { "views" }
    | VOLATILE { "volatile" }
    | WHITESPACE_P { "whitespace" }
    | WITHIN { "within" }
    | WITHOUT { "without" }
    | WORK { "work" }
    | WRAPPER { "wrapper" }
    | WRITE { "write" }
    | XML_P { "xml" }
    | YEAR_P { "year" }
    | YES_P { "yes" }
    | ZONE { "zone" }

-- * Column identifier -- *- keywords that can be column, table, etc names.
-- *
-- * Many of these keywords will in fact be recognized as type or function
-- * names too; but they have special productions for the purpose, and so
-- * can't be treated as "generic" type or function names.
-- *
-- * The type names appearing here are not usable as function names
-- * because they can be followed by '(' in typename productions, which
-- * looks too much like a function call for an LR(1) parser.
col_name_keyword :: { Text }
    : BETWEEN { "between" }
    | BIGINT { "bigint" }
    | BIT { "bit" }
    | BOOLEAN_P { "boolean" }
    | CHAR_P { "char" }
    | CHARACTER { "character" }
    | COALESCE { "coalesce" }
    | DEC { "dec" }
    | DECIMAL_P { "decimal" }
    | EXISTS { "exists" }
    | EXTRACT { "extract" }
    | FLOAT_P { "float" }
    | GREATEST { "greatest" }
    | GROUPING { "grouping" }
    | INOUT { "inout" }
    | INT_P { "int" }
    | INTEGER { "integer" }
    | INTERVAL { "interval" }
    | LEAST { "least" }
    | NATIONAL { "national" }
    | NCHAR { "nchar" }
    | NONE { "none" }
    | NULLIF { "nullif" }
    | NUMERIC { "numeric" }
    | OUT_P { "out" }
    | OVERLAY { "overlay" }
    | POSITION { "position" }
    | PRECISION { "precision" }
    | REAL { "real" }
    | ROW { "row" }
    | SETOF { "setof" }
    | SMALLINT { "smallint" }
    | SUBSTRING { "substring" }
    | TIME { "time" }
    | TIMESTAMP { "timestamp" }
    | TREAT { "treat" }
    | TRIM { "trim" }
    | VALUES { "values" }
    | VARCHAR { "varchar" }
    | XMLATTRIBUTES { "xmlattributes" }
    | XMLCONCAT { "xmlconcat" }
    | XMLELEMENT { "xmlelement" }
    | XMLEXISTS { "xmlexists" }
    | XMLFOREST { "xmlforest" }
    | XMLNAMESPACES { "xmlnamespaces" }
    | XMLPARSE { "xmlparse" }
    | XMLPI { "xmlpi" }
    | XMLROOT { "xmlroot" }
    | XMLSERIALIZE { "xmlserialize" }
    | XMLTABLE { "xmltable" }

-- * Type/function identifier -- *- keywords that can be type or function names.
-- *
-- * Most of these are keywords that are used as operators in expressions;
-- * in general such keywords can't be column names because they would be
-- * ambiguous with variables, but they are unambiguous as function identifiers.
-- *
-- * Do not include POSITION, SUBSTRING, etc here since they have explicit
-- * productions in a_expr to support the goofy SQL9x argument syntax.
-- * - thomas 2000-11-28
type_func_name_keyword :: { Text }
			: AUTHORIZATION { "authorization" }
			| BINARY { "binary" }
			| COLLATION { "collation" }
			| CONCURRENTLY { "concurrently" }
			| CROSS { "cross" }
			| CURRENT_SCHEMA { "current_schema" }
			| FREEZE { "freeze" }
			| FULL { "full" }
			| ILIKE { "ilike" }
			| INNER_P { "inner" }
			| IS { "is" }
			| ISNULL { "isnull" }
			| JOIN { "join" }
			| LEFT { "left" }
			| LIKE { "like" }
			| NATURAL { "natural" }
			| NOTNULL { "notnull" }
			| OUTER_P { "outer" }
			| OVERLAPS { "overlaps" }
			| RIGHT { "right" }
			| SIMILAR { "similar" }
			| TABLESAMPLE { "tablesample" }
			| VERBOSE { "verbose" }

-- * Reserved keyword -- *- these keywords are usable only as a ColLabel.
-- *
-- * Keywords appear here if they could not be distinguished from variable,
-- * type, or function names in some contexts.  Don't put things here unless
-- * forced to.
reserved_keyword :: { Text }
			: ALL { "all" }
			| ANALYSE { "analyse" }
			| ANALYZE { "analyze" }
			| AND { "and" }
			| ANY { "any" }
			| ARRAY { "array" }
			| AS { "as" }
			| ASC { "asc" }
			| ASYMMETRIC { "asymmetric" }
			| BOTH { "both" }
			| CASE { "case" }
			| CAST { "cast" }
			| CHECK { "check" }
			| COLLATE { "collate" }
			| COLUMN { "column" }
			| CONSTRAINT { "constraint" }
			| CREATE { "create" }
			| CURRENT_CATALOG { "current_catalog" }
			| CURRENT_DATE { "current_date" }
			| CURRENT_ROLE { "current_role" }
			| CURRENT_TIME { "current_time" }
			| CURRENT_TIMESTAMP { "current_timestamp" }
			| CURRENT_USER { "current_user" }
			| DEFAULT { "default" }
			| DEFERRABLE { "deferrable" }
			| DESC { "desc" }
			| DISTINCT { "distinct" }
			| DO { "do" }
			| ELSE { "else" }
			| END_P { "end" }
			| EXCEPT { "except" }
			| FALSE_P { "false" }
			| FETCH { "fetch" }
			| FOR { "for" }
			| FOREIGN { "foreign" }
			| FROM { "from" }
			| GRANT { "grant" }
			| GROUP_P { "group" }
			| HAVING { "having" }
			| IN_P { "in" }
			| INITIALLY { "initially" }
			| INTERSECT { "intersect" }
			| INTO { "into" }
			| LATERAL_P { "lateral" }
			| LEADING { "leading" }
			| LIMIT { "limit" }
			| LOCALTIME { "localtime" }
			| LOCALTIMESTAMP { "localtimestamp" }
			| NOT { "not" }
			| NULL_P { "null" }
			| OFFSET { "offset" }
			| ON { "on" }
			| ONLY { "only" }
			| OR { "or" }
			| ORDER { "order" }
			| PLACING { "placing" }
			| PRIMARY { "primary" }
			| REFERENCES { "references" }
			| RETURNING { "returning" }
			| SELECT { "select" }
			| SESSION_USER { "current_user" }
			| SOME { "some" }
			| SYMMETRIC { "symmetric" }
			| TABLE { "table" }
			| THEN { "then" }
			| TO { "to" }
			| TRAILING { "trailing" }
			| TRUE_P { "true" }
			| UNION { "union" }
			| UNIQUE { "unique" }
			| USER { "user" }
			| USING { "using" }
			| VARIADIC { "variadic" }
			| WHEN { "when" }
			| WHERE { "where" }
			| WINDOW { "window" }
			| WITH { "with" }

{

-- from https://github.com/dagit/happy-plus-alex/blob/master/src/Parser.y

happyError :: L.LocToken -> Alex a
happyError (L.LocToken p t) =
  L.alexErrorPosn p ("parse error at token '" ++ L.unLex t ++ "'")

parseStatement :: FilePath -> String -> Either String Statement
parseStatement = L.runAlexWithFilepath parseStatement_

parseSelect :: FilePath -> String -> Either String SelectStmt
parseSelect = L.runAlexWithFilepath parseSelect_

parseExpr :: FilePath -> String -> Either String Expr
parseExpr = L.runAlexWithFilepath parseExpr_

lexwrap :: (L.LocToken -> Alex a) -> Alex a
lexwrap = (L.alexMonadScan' >>=)

}
