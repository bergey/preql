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

 -- * NOTES
 -- *	  CAPITALS are used to represent terminal symbols.
 -- *	  non-capitals are used to represent non-terminals.

-- This Haskell port generally follows the convention above, taken from the PostgreSQL bison source.
-- Comments with a leading * are taken from the PostgreSQL source.
-- Unimplemnted parts of the official parser are marked TODO, and generally contain bison & C syntax.

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
    LAST_P { L.LocToken _ L.LAST_P }
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
    : select_no_parens { $1 }
    | select_with_parens { $1 }

select_with_parens
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
    : simple_select { SimpleSelect $1 }
    | select_clause sort_clause { SortedSelect $1 $2 }
    -- TODO            | select_clause opt_sort_clause for_locking_clause opt_select_limit
-- TODO                {
-- TODO                    insertSelectOptions((SelectStmt *) $1, $2, $3,
-- TODO                                        list_nth($4, 0), list_nth($4, 1),
-- TODO                                        NULL,
-- TODO                                        yyscanner);
-- TODO                    $$ = $1;
-- TODO                }
-- TODO            | select_clause opt_sort_clause select_limit opt_for_locking_clause
-- TODO                {
-- TODO                    insertSelectOptions((SelectStmt *) $1, $2, $4,
-- TODO                                        list_nth($3, 0), list_nth($3, 1),
-- TODO                                        NULL,
-- TODO                                        yyscanner);
-- TODO                    $$ = $1;
-- TODO                }
-- TODO            | with_clause select_clause
-- TODO                {
-- TODO                    insertSelectOptions((SelectStmt *) $2, NULL, NIL,
-- TODO                                        NULL, NULL,
-- TODO                                        $1,
-- TODO                                        yyscanner);
-- TODO                    $$ = $2;
-- TODO                }
-- TODO            | with_clause select_clause sort_clause
-- TODO                {
-- TODO                    insertSelectOptions((SelectStmt *) $2, $3, NIL,
-- TODO                                        NULL, NULL,
-- TODO                                        $1,
-- TODO                                        yyscanner);
-- TODO                    $$ = $2;
-- TODO                }
-- TODO            | with_clause select_clause opt_sort_clause for_locking_clause opt_select_limit
-- TODO                {
-- TODO                    insertSelectOptions((SelectStmt *) $2, $3, $4,
-- TODO                                        list_nth($5, 0), list_nth($5, 1),
-- TODO                                        $1,
-- TODO                                        yyscanner);
-- TODO                    $$ = $2;
-- TODO                }
-- TODO            | with_clause select_clause opt_sort_clause select_limit opt_for_locking_clause
-- TODO                {
-- TODO                    insertSelectOptions((SelectStmt *) $2, $3, $5,
-- TODO                                        list_nth($4, 0), list_nth($4, 1),
-- TODO                                        $1,
-- TODO                                        yyscanner);
-- TODO                    $$ = $2;
-- TODO                }
-- TODO        ;

select_clause :: { SelectStmt }
    : simple_select                            { SimpleSelect $1 }
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

simple_select :: { SimpleSelect }
           : SELECT opt_all_clause opt_target_list
           into_clause from_clause where_clause
           group_clause having_clause window_clause { SelectUnordered (Unordered Nothing $3) }
-- TODO WIP
-- TODO                {
-- TODO                    SelectStmt *n = makeNode(SelectStmt);
-- TODO                    n->targetList = $3;
-- TODO                    n->intoClause = $4;
-- TODO                    n->fromClause = $5;
-- TODO                    n->whereClause = $6;
-- TODO                    n->groupClause = $7;
-- TODO                    n->havingClause = $8;
-- TODO                    n->windowClause = $9;
-- TODO                    $$ = (Node *)n;
-- TODO                }
-- TODO            | SELECT distinct_clause target_list
-- TODO            into_clause from_clause where_clause
-- TODO            group_clause having_clause window_clause
-- TODO                {
-- TODO                    SelectStmt *n = makeNode(SelectStmt);
-- TODO                    n->distinctClause = $2;
-- TODO                    n->targetList = $3;
-- TODO                    n->intoClause = $4;
-- TODO                    n->fromClause = $5;
-- TODO                    n->whereClause = $6;
-- TODO                    n->groupClause = $7;
-- TODO                    n->havingClause = $8;
-- TODO                    n->windowClause = $9;
-- TODO                    $$ = (Node *)n;
-- TODO                }
            | values_clause                            { SelectValues $1 }
-- TODO TODO select * in AST
-- TODO            | TABLE relation_expr
-- TODO                {
-- TODO                    /* same as SELECT * FROM relation_expr */
-- TODO                    ColumnRef *cr = makeNode(ColumnRef);
-- TODO                    ResTarget *rt = makeNode(ResTarget);
-- TODO                    SelectStmt *n = makeNode(SelectStmt);
-- TODO
-- TODO                    cr->fields = list_make1(makeNode(A_Star));
-- TODO                    cr->location = -1;
-- TODO
-- TODO                    rt->name = NULL;
-- TODO                    rt->indirection = NIL;
-- TODO                    rt->val = (Node *)cr;
-- TODO                    rt->location = -1;
-- TODO
-- TODO                    n->targetList = list_make1(rt);
-- TODO                    n->fromClause = list_make1($2);
-- TODO                    $$ = (Node *)n;
-- TODO                }
-- TODO TODO UNION in AST
-- TODO            | select_clause UNION all_or_distinct select_clause
-- TODO                {
-- TODO                    $$ = makeSetOp(SETOP_UNION, $3, $1, $4);
-- TODO                }
-- TODO            | select_clause INTERSECT all_or_distinct select_clause
-- TODO                {
-- TODO                    $$ = makeSetOp(SETOP_INTERSECT, $3, $1, $4);
-- TODO                }
-- TODO            | select_clause EXCEPT all_or_distinct select_clause
-- TODO                {
-- TODO                    $$ = makeSetOp(SETOP_EXCEPT, $3, $1, $4);
-- TODO                }

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

-- * We should allow ROW '(' expr_list ')' too, but that seems to require
-- * making VALUES a fully reserved word, which will probably break more apps
-- * than allowing the noise-word is worth.
values_clause
    : VALUES '(' expr_list ')' { NE.fromList (reverse $3) :| [] }
    | values_clause COMMA '(' expr_list ')' { NE.cons (NE.fromList (reverse $4)) $1 }

 -- *	clauses common to all Optimizable Stmts:
 -- *		from_clause		- allow list of both JOIN expressions and table names
 -- *		where_clause	- qualifications for joins or restrictions
 
from_clause :: { [TableRef] }
    : FROM from_list { reverse $1 }
	| 							{ [] }

from_list : list(table_ref) { $1 }

-- * table_ref is where an alias clause can be attached.
table_ref :: { TableRef }
    :	relation_expr opt_alias_clause { TableRef $1 $2 }
-- TODO				{
-- TODO					$1->alias = $2;
-- TODO					$$ = (Node *) $1;
-- TODO				}
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
-- TODO			| select_with_parens opt_alias_clause
-- TODO				{
-- TODO					RangeSubselect *n = makeNode(RangeSubselect);
-- TODO					n->lateral = false;
-- TODO					n->subquery = $1;
-- TODO					n->alias = $2;
-- TODO					/*
-- TODO					 * The SQL spec does not permit a subselect
-- TODO					 * (<derived_table>) without an alias clause,
-- TODO					 * so we don't either.  This avoids the problem
-- TODO					 * of needing to invent a unique refname for it.
-- TODO					 * That could be surmounted if there's sufficient
-- TODO					 * popular demand, but for now let's just implement
-- TODO					 * the spec and see if anyone complains.
-- TODO					 * However, it does seem like a good idea to emit
-- TODO					 * an error message that's better than "syntax error".
-- TODO					 */
-- TODO					if ($2 == NULL)
-- TODO					{
-- TODO						if (IsA($1, SelectStmt) &&
-- TODO							((SelectStmt *) $1)->valuesLists)
-- TODO							ereport(ERROR,
-- TODO									(errcode(ERRCODE_SYNTAX_ERROR),
-- TODO									 errmsg("VALUES in FROM must have an alias"),
-- TODO									 errhint("For example, FROM (VALUES ...) [AS] foo."),
-- TODO									 parser_errposition(@1)));
-- TODO						else
-- TODO							ereport(ERROR,
-- TODO									(errcode(ERRCODE_SYNTAX_ERROR),
-- TODO									 errmsg("subquery in FROM must have an alias"),
-- TODO									 errhint("For example, FROM (SELECT ...) [AS] foo."),
-- TODO									 parser_errposition(@1)));
-- TODO					}
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
-- TODO			| joined_table
-- TODO				{
-- TODO					$$ = (Node *) $1;
-- TODO				}
-- TODO			| '(' joined_table ')' alias_clause
-- TODO				{
-- TODO					$2->alias = $4;
-- TODO					$$ = (Node *) $2;
-- TODO }

-- TODO joined_table:
-- TODO alias_clause:
-- TODO opt_alias_clause: 
-- TODO func_alias_clause:
-- TODO join_type:	
-- TODO join_outer:
-- TODO join_qual:

relation_expr :: { Name } -- TODO FIXME
    : qualified_name { $1 } -- * inheritance query, implicitly
-- TODO 			| qualified_name '*'
-- TODO 				{
-- TODO 					/* inheritance query, explicitly */
-- TODO 					$$ = $1;
-- TODO 					$$->inh = true;
-- TODO 					$$->alias = NULL;
-- TODO 				}
-- TODO 			| ONLY qualified_name
-- TODO 				{
-- TODO 					/* no inheritance */
-- TODO 					$$ = $2;
-- TODO 					$$->inh = false;
-- TODO 					$$->alias = NULL;
-- TODO 				}
-- TODO 			| ONLY '(' qualified_name ')'
-- TODO 				{
-- TODO 					/* no inheritance, SQL99-style syntax */
-- TODO 					$$ = $3;
-- TODO 					$$->inh = false;
-- TODO 					$$->alias = NULL;
-- TODO 				}

relation_expr_list : list(relation_expr) { $1 }

-- FIXME handwritten
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

-- * We use (DistinctAll) as a placeholder to indicate that all target expressions
-- * should be placed in the DISTINCT list during parsetree analysis.
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

 -- *	target list for SELECT

opt_target_list :: { [ResTarget] }
    : target_list { NE.toList $1 }
    | { [] }

target_list : list(target_el) { NE.fromList (reverse $1) }

target_el :: { ResTarget }
    : a_expr AS ColLabel { ColumnTarget (ColumnRef $1 (Just $3)) }
    | a_expr IDENT { ColumnTarget (ColumnRef $1 (Just $2)) }
    | a_expr { ColumnTarget (ColumnRef $1 Nothing) }
    | '*' { Star }

-- * Name classification hierarchy.
-- *
-- * IDENT is the lexeme returned by the lexer for identifiers that match
-- * no known keyword.  In most cases, we can accept certain keywords as
-- * names, not only IDENTs.	We prefer to accept as many such keywords
-- * as possible to minimize the impact of "reserved words" on programmers.
-- * So, we divide names into several possible classes.  The classification
-- * is chosen in part to make keywords acceptable as names wherever possible.

-- Column identifier -- *- names that can be column, table, etc names.
ColId
    :		IDENT									{ $1 }
    | unreserved_keyword					{ $1 }

-- * Type/function identifier -- *- names that can be type or function names.
type_function_name
    :	IDENT							{ $1 }
    | unreserved_keyword					{ $1 }
    | type_func_name_keyword				{ $1 }

-- * Any not-fully-reserved word -- *- these names can be, eg, role names.
NonReservedWord
     :	IDENT							{ $1 }
			| unreserved_keyword					{ $1 }
			| col_name_keyword						{ $1 }
			| type_func_name_keyword				{ $1 }

-- * Column label -- *- allowed labels in "AS" clauses.
-- * This presently includes *all* Postgres keywords.
ColLabel:	IDENT									{  $1 }
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
unreserved_keyword :: { Name }
    : ABORT_P { Name "abort" }
    | ABSOLUTE_P { Name "absolute" }
    | ACCESS { Name "access" }
    | ACTION { Name "action" }
    | ADD_P { Name "add" }
    | ADMIN { Name "admin" }
    | AFTER { Name "after" }
    | AGGREGATE { Name "aggregate" }
    | ALSO { Name "also" }
    | ALTER { Name "alter" }
    | ALWAYS { Name "always" }
    | ASSERTION { Name "assertion" }
    | ASSIGNMENT { Name "assignment" }
    | AT { Name "at" }
    | ATTACH { Name "attach" }
    | ATTRIBUTE { Name "attribute" }
    | BACKWARD { Name "backward" }
    | BEFORE { Name "before" }
    | BEGIN_P { Name "begin" }
    | BY { Name "by" }
    | CACHE { Name "cache" }
    | CALL { Name "call" }
    | CALLED { Name "called" }
    | CASCADE { Name "cascade" }
    | CASCADED { Name "cascaded" }
    | CATALOG_P { Name "catalog" }
    | CHAIN { Name "chain" }
    | CHARACTERISTICS { Name "characteristics" }
    | CHECKPOINT { Name "checkpoint" }
    | CLASS { Name "class" }
    | CLOSE { Name "close" }
    | CLUSTER { Name "cluster" }
    | COLUMNS { Name "columns" }
    | COMMENT { Name "comment" }
    | COMMENTS { Name "comments" }
    | COMMIT { Name "commit" }
    | COMMITTED { Name "committed" }
    | CONFIGURATION { Name "configuration" }
    | CONFLICT { Name "conflict" }
    | CONNECTION { Name "connection" }
    | CONSTRAINTS { Name "constraints" }
    | CONTENT_P { Name "content" }
    | CONTINUE_P { Name "continue" }
    | CONVERSION_P { Name "conversion" }
    | COPY { Name "copy" }
    | COST { Name "cost" }
    | CSV { Name "csv" }
    | CUBE { Name "cube" }
    | CURRENT_P { Name "current" }
    | CURSOR { Name "cursor" }
    | CYCLE { Name "cycle" }
    | DATA_P { Name "data" }
    | DATABASE { Name "database" }
    | DAY_P { Name "day" }
    | DEALLOCATE { Name "deallocate" }
    | DECLARE { Name "declare" }
    | DEFAULTS { Name "defaults" }
    | DEFERRED { Name "deferred" }
    | DEFINER { Name "definer" }
    | DELETE_P { Name "delete" }
    | DELIMITER { Name "delimiter" }
    | DELIMITERS { Name "delimiters" }
    | DEPENDS { Name "depends" }
    | DETACH { Name "detach" }
    | DICTIONARY { Name "dictionary" }
    | DISABLE_P { Name "disable" }
    | DISCARD { Name "discard" }
    | DOCUMENT_P { Name "document" }
    | DOMAIN_P { Name "domain" }
    | DOUBLE_P { Name "double" }
    | DROP { Name "drop" }
    | EACH { Name "each" }
    | ENABLE_P { Name "enable" }
    | ENCODING { Name "encoding" }
    | ENCRYPTED { Name "encrypted" }
    | ENUM_P { Name "enum" }
    | ESCAPE { Name "escape" }
    | EVENT { Name "event" }
    | EXCLUDE { Name "exclude" }
    | EXCLUDING { Name "excluding" }
    | EXCLUSIVE { Name "exclusive" }
    | EXECUTE { Name "execute" }
    | EXPLAIN { Name "explain" }
    | EXTENSION { Name "extension" }
    | EXTERNAL { Name "external" }
    | FAMILY { Name "family" }
    | FILTER { Name "filter" }
    | FIRST_P { Name "first" }
    | FOLLOWING { Name "following" }
    | FORCE { Name "force" }
    | FORWARD { Name "forward" }
    | FUNCTION { Name "function" }
    | FUNCTIONS { Name "functions" }
    | GENERATED { Name "generated" }
    | GLOBAL { Name "global" }
    | GRANTED { Name "granted" }
    | GROUPS { Name "groups" }
    | HANDLER { Name "handler" }
    | HEADER_P { Name "header" }
    | HOLD { Name "hold" }
    | HOUR_P { Name "hour" }
    | IDENTITY_P { Name "identity" }
    | IF_P { Name "if" }
    | IMMEDIATE { Name "immediate" }
    | IMMUTABLE { Name "immutable" }
    | IMPLICIT_P { Name "implicit" }
    | IMPORT_P { Name "import" }
    | INCLUDE { Name "include" }
    | INCLUDING { Name "including" }
    | INCREMENT { Name "increment" }
    | INDEX { Name "index" }
    | INDEXES { Name "indexes" }
    | INHERIT { Name "inherit" }
    | INHERITS { Name "inherits" }
    | INLINE_P { Name "inline" }
    | INPUT_P { Name "input" }
    | INSENSITIVE { Name "insensitive" }
    | INSERT { Name "insert" }
    | INSTEAD { Name "instead" }
    | INVOKER { Name "invoker" }
    | ISOLATION { Name "isolation" }
    | KEY { Name "key" }
    | LABEL { Name "label" }
    | LANGUAGE { Name "language" }
    | LARGE_P { Name "large" }
    | LAST_P { Name "last" }
    | LEAKPROOF { Name "leakproof" }
    | LEVEL { Name "level" }
    | LISTEN { Name "listen" }
    | LOAD { Name "load" }
    | LOCAL { Name "local" }
    | LOCATION { Name "location" }
    | LOCK_P { Name "lock" }
    | LOCKED { Name "locked" }
    | LOGGED { Name "logged" }
    | MAPPING { Name "mapping" }
    | MATCH { Name "match" }
    | MATERIALIZED { Name "materialized" }
    | MAXVALUE { Name "maxvalue" }
    | METHOD { Name "method" }
    | MINUTE_P { Name "minute" }
    | MINVALUE { Name "minvalue" }
    | MODE { Name "mode" }
    | MONTH_P { Name "month" }
    | MOVE { Name "move" }
    | NAME_P { Name "name" }
    | NAMES { Name "names" }
    | NEW { Name "new" }
    | NEXT { Name "next" }
    | NO { Name "no" }
    | NOTHING { Name "nothing" }
    | NOTIFY { Name "notify" }
    | NOWAIT { Name "nowait" }
    | NULLS_P { Name "nulls" }
    | OBJECT_P { Name "object" }
    | OF { Name "of" }
    | OFF { Name "off" }
    | OIDS { Name "oids" }
    | OLD { Name "old" }
    | OPERATOR { Name "operator" }
    | OPTION { Name "option" }
    | OPTIONS { Name "options" }
    | ORDINALITY { Name "ordinality" }
    | OTHERS { Name "others" }
    | OVER { Name "over" }
    | OVERRIDING { Name "overriding" }
    | OWNED { Name "owned" }
    | OWNER { Name "owner" }
    | PARALLEL { Name "parallel" }
    | PARSER { Name "parser" }
    | PARTIAL { Name "partial" }
    | PARTITION { Name "partition" }
    | PASSING { Name "passing" }
    | PASSWORD { Name "password" }
    | PLANS { Name "plans" }
    | POLICY { Name "policy" }
    | PRECEDING { Name "preceding" }
    | PREPARE { Name "prepare" }
    | PREPARED { Name "prepared" }
    | PRESERVE { Name "preserve" }
    | PRIOR { Name "prior" }
    | PRIVILEGES { Name "privileges" }
    | PROCEDURAL { Name "procedural" }
    | PROCEDURE { Name "procedure" }
    | PROCEDURES { Name "procedures" }
    | PROGRAM { Name "program" }
    | PUBLICATION { Name "publication" }
    | QUOTE { Name "quote" }
    | RANGE { Name "range" }
    | READ { Name "read" }
    | REASSIGN { Name "reassign" }
    | RECHECK { Name "recheck" }
    | RECURSIVE { Name "recursive" }
    | REF { Name "ref" }
    | REFERENCING { Name "referencing" }
    | REFRESH { Name "refresh" }
    | REINDEX { Name "reindex" }
    | RELATIVE_P { Name "relative" }
    | RELEASE { Name "release" }
    | RENAME { Name "rename" }
    | REPEATABLE { Name "repeatable" }
    | REPLACE { Name "replace" }
    | REPLICA { Name "replica" }
    | RESET { Name "reset" }
    | RESTART { Name "restart" }
    | RESTRICT { Name "restrict" }
    | RETURNS { Name "returns" }
    | REVOKE { Name "revoke" }
    | ROLE { Name "role" }
    | ROLLBACK { Name "rollback" }
    | ROLLUP { Name "rollup" }
    | ROUTINE { Name "routine" }
    | ROUTINES { Name "routines" }
    | ROWS { Name "rows" }
    | RULE { Name "rule" }
    | SAVEPOINT { Name "savepoint" }
    | SCHEMA { Name "schema" }
    | SCHEMAS { Name "schemas" }
    | SCROLL { Name "scroll" }
    | SEARCH { Name "search" }
    | SECOND_P { Name "second" }
    | SECURITY { Name "security" }
    | SEQUENCE { Name "sequence" }
    | SEQUENCES { Name "sequences" }
    | SERIALIZABLE { Name "serializable" }
    | SERVER { Name "server" }
    | SESSION { Name "session" }
    | SET { Name "set" }
    | SETS { Name "sets" }
    | SHARE { Name "share" }
    | SHOW { Name "show" }
    | SIMPLE { Name "simple" }
    | SKIP { Name "skip" }
    | SNAPSHOT { Name "snapshot" }
    | SQL_P { Name "sql" }
    | STABLE { Name "stable" }
    | STANDALONE_P { Name "standalone" }
    | START { Name "start" }
    | STATEMENT { Name "statement" }
    | STATISTICS { Name "statistics" }
    | STDIN { Name "stdin" }
    | STDOUT { Name "stdout" }
    | STORAGE { Name "storage" }
    | STORED { Name "stored" }
    | STRICT_P { Name "strict" }
    | STRIP_P { Name "strip" }
    | SUBSCRIPTION { Name "subscription" }
    | SUPPORT { Name "support" }
    | SYSID { Name "sysid" }
    | SYSTEM_P { Name "system" }
    | TABLES { Name "tables" }
    | TABLESPACE { Name "tablespace" }
    | TEMP { Name "temp" }
    | TEMPLATE { Name "template" }
    | TEMPORARY { Name "temporary" }
    | TEXT_P { Name "text" }
    | TIES { Name "ties" }
    | TRANSACTION { Name "transaction" }
    | TRANSFORM { Name "transform" }
    | TRIGGER { Name "trigger" }
    | TRUNCATE { Name "truncate" }
    | TRUSTED { Name "trusted" }
    | TYPE_P { Name "type" }
    | TYPES_P { Name "types" }
    | UNBOUNDED { Name "unbounded" }
    | UNCOMMITTED { Name "uncommitted" }
    | UNENCRYPTED { Name "unencrypted" }
    | UNKNOWN { Name "unknown" }
    | UNLISTEN { Name "unlisten" }
    | UNLOGGED { Name "unlogged" }
    | UNTIL { Name "until" }
    | UPDATE { Name "update" }
    | VACUUM { Name "vacuum" }
    | VALID { Name "valid" }
    | VALIDATE { Name "validate" }
    | VALIDATOR { Name "validator" }
    | VALUE_P { Name "value" }
    | VARYING { Name "varying" }
    | VERSION_P { Name "version" }
    | VIEW { Name "view" }
    | VIEWS { Name "views" }
    | VOLATILE { Name "volatile" }
    | WHITESPACE_P { Name "whitespace" }
    | WITHIN { Name "within" }
    | WITHOUT { Name "without" }
    | WORK { Name "work" }
    | WRAPPER { Name "wrapper" }
    | WRITE { Name "write" }
    | XML_P { Name "xml" }
    | YEAR_P { Name "year" }
    | YES_P { Name "yes" }
    | ZONE { Name "zone" }

-- * Column identifier -- *- keywords that can be column, table, etc names.
-- *
-- * Many of these keywords will in fact be recognized as type or function
-- * names too; but they have special productions for the purpose, and so
-- * can't be treated as "generic" type or function names.
-- *
-- * The type names appearing here are not usable as function names
-- * because they can be followed by '(' in typename productions, which
-- * looks too much like a function call for an LR(1) parser.
col_name_keyword :: { Name }
    : BETWEEN { Name "between" }
    | BIGINT { Name "bigint" }
    | BIT { Name "bit" }
    | BOOLEAN_P { Name "boolean" }
    | CHAR_P { Name "char" }
    | CHARACTER { Name "character" }
    | COALESCE { Name "coalesce" }
    | DEC { Name "dec" }
    | DECIMAL_P { Name "decimal" }
    | EXISTS { Name "exists" }
    | EXTRACT { Name "extract" }
    | FLOAT_P { Name "float" }
    | GREATEST { Name "greatest" }
    | GROUPING { Name "grouping" }
    | INOUT { Name "inout" }
    | INT_P { Name "int" }
    | INTEGER { Name "integer" }
    | INTERVAL { Name "interval" }
    | LEAST { Name "least" }
    | NATIONAL { Name "national" }
    | NCHAR { Name "nchar" }
    | NONE { Name "none" }
    | NULLIF { Name "nullif" }
    | NUMERIC { Name "numeric" }
    | OUT_P { Name "out" }
    | OVERLAY { Name "overlay" }
    | POSITION { Name "position" }
    | PRECISION { Name "precision" }
    | REAL { Name "real" }
    | ROW { Name "row" }
    | SETOF { Name "setof" }
    | SMALLINT { Name "smallint" }
    | SUBSTRING { Name "substring" }
    | TIME { Name "time" }
    | TIMESTAMP { Name "timestamp" }
    | TREAT { Name "treat" }
    | TRIM { Name "trim" }
    | VALUES { Name "values" }
    | VARCHAR { Name "varchar" }
    | XMLATTRIBUTES { Name "xmlattributes" }
    | XMLCONCAT { Name "xmlconcat" }
    | XMLELEMENT { Name "xmlelement" }
    | XMLEXISTS { Name "xmlexists" }
    | XMLFOREST { Name "xmlforest" }
    | XMLNAMESPACES { Name "xmlnamespaces" }
    | XMLPARSE { Name "xmlparse" }
    | XMLPI { Name "xmlpi" }
    | XMLROOT { Name "xmlroot" }
    | XMLSERIALIZE { Name "xmlserialize" }
    | XMLTABLE { Name "xmltable" }

-- * Type/function identifier -- *- keywords that can be type or function names.
-- *
-- * Most of these are keywords that are used as operators in expressions;
-- * in general such keywords can't be column names because they would be
-- * ambiguous with variables, but they are unambiguous as function identifiers.
-- *
-- * Do not include POSITION, SUBSTRING, etc here since they have explicit
-- * productions in a_expr to support the goofy SQL9x argument syntax.
-- * - thomas 2000-11-28
type_func_name_keyword :: { Name }
			: AUTHORIZATION { Name "authorization" }
			| BINARY { Name "binary" }
			| COLLATION { Name "collation" }
			| CONCURRENTLY { Name "concurrently" }
			| CROSS { Name "cross" }
			| CURRENT_SCHEMA { Name "current_schema" }
			| FREEZE { Name "freeze" }
			| FULL { Name "full" }
			| ILIKE { Name "ilike" }
			| INNER_P { Name "inner" }
			| IS { Name "is" }
			| ISNULL { Name "isnull" }
			| JOIN { Name "join" }
			| LEFT { Name "left" }
			| LIKE { Name "like" }
			| NATURAL { Name "natural" }
			| NOTNULL { Name "notnull" }
			| OUTER_P { Name "outer" }
			| OVERLAPS { Name "overlaps" }
			| RIGHT { Name "right" }
			| SIMILAR { Name "similar" }
			| TABLESAMPLE { Name "tablesample" }
			| VERBOSE { Name "verbose" }

-- * Reserved keyword -- *- these keywords are usable only as a ColLabel.
-- *
-- * Keywords appear here if they could not be distinguished from variable,
-- * type, or function names in some contexts.  Don't put things here unless
-- * forced to.
reserved_keyword :: { Name }
			: ALL { Name "all" }
			| ANALYSE { Name "analyse" }
			| ANALYZE { Name "analyze" }
			| AND { Name "and" }
			| ANY { Name "any" }
			| ARRAY { Name "array" }
			| AS { Name "as" }
			| ASC { Name "asc" }
			| ASYMMETRIC { Name "asymmetric" }
			| BOTH { Name "both" }
			| CASE { Name "case" }
			| CAST { Name "cast" }
			| CHECK { Name "check" }
			| COLLATE { Name "collate" }
			| COLUMN { Name "column" }
			| CONSTRAINT { Name "constraint" }
			| CREATE { Name "create" }
			| CURRENT_CATALOG { Name "current_catalog" }
			| CURRENT_DATE { Name "current_date" }
			| CURRENT_ROLE { Name "current_role" }
			| CURRENT_TIME { Name "current_time" }
			| CURRENT_TIMESTAMP { Name "current_timestamp" }
			| CURRENT_USER { Name "current_user" }
			| DEFAULT { Name "default" }
			| DEFERRABLE { Name "deferrable" }
			| DESC { Name "desc" }
			| DISTINCT { Name "distinct" }
			| DO { Name "do" }
			| ELSE { Name "else" }
			| END_P { Name "end" }
			| EXCEPT { Name "except" }
			| FALSE_P { Name "false" }
			| FETCH { Name "fetch" }
			| FOR { Name "for" }
			| FOREIGN { Name "foreign" }
			| FROM { Name "from" }
			| GRANT { Name "grant" }
			| GROUP_P { Name "group" }
			| HAVING { Name "having" }
			| IN_P { Name "in" }
			| INITIALLY { Name "initially" }
			| INTERSECT { Name "intersect" }
			| INTO { Name "into" }
			| LATERAL_P { Name "lateral" }
			| LEADING { Name "leading" }
			| LIMIT { Name "limit" }
			| LOCALTIME { Name "localtime" }
			| LOCALTIMESTAMP { Name "localtimestamp" }
			| NOT { Name "not" }
			| NULL_P { Name "null" }
			| OFFSET { Name "offset" }
			| ON { Name "on" }
			| ONLY { Name "only" }
			| OR { Name "or" }
			| ORDER { Name "order" }
			| PLACING { Name "placing" }
			| PRIMARY { Name "primary" }
			| REFERENCES { Name "references" }
			| RETURNING { Name "returning" }
			| SELECT { Name "select" }
			| SESSION_USER { Name "current_user" }
			| SOME { Name "some" }
			| SYMMETRIC { Name "symmetric" }
			| TABLE { Name "table" }
			| THEN { Name "then" }
			| TO { Name "to" }
			| TRAILING { Name "trailing" }
			| TRUE_P { Name "true" }
			| UNION { Name "union" }
			| UNIQUE { Name "unique" }
			| USER { Name "user" }
			| USING { Name "using" }
			| VARIADIC { Name "variadic" }
			| WHEN { Name "when" }
			| WHERE { Name "where" }
			| WINDOW { Name "window" }
			| WITH { Name "with" }

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
