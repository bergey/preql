{
module Untyped.Lex where

import           Data.Text (Text)
import           Prelude hiding (LT, GT, lex)

import qualified Data.Text as T

}

%wrapper "monadUserState"

$unicodeIds = $printable # [$white \,\.\;\'\"\(\)\<\>=\+\-\^\!@]
$firstLetter = $unicodeIds # [0-9_\$]
$quoted = $printable # [\']
$digit = [0-9]
$haskell = $printable # [\}]
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
    $a $s $c { lex ASC }
    $d $e $s $c { lex DESC }
    $o $r $d $e $r { lex ORDER }
    $b $y { lex BY }
    $u $s $i $n $g { lex USING }
    $o $p $e $r $a $t $o $r { lex OPERATOR }
    $u $n $i $o $n { lex UNION }
    $e $x $c $e $p $t { lex EXCEPT }
    $n $u $l $l $s { lex Nulls }
    $f $i $r $s $t { lex First }
    $l $a $s $t { lex Last }
    $a $l $l  {lex ALL }
    $d $i $s $t $i $n $c $t { lex DISTINCT }
    $o $n { lex ON }
    $a $s { lex AS }
    $d $e $l $e $t $e { lex DELETE_P }
    $s $e $l $e $c $t { lex SELECT }
    $i $n $s $e $r $t { lex INSERT }
    $u $p $d $a $t $e { lex UPDATE }
    $f $r $o $m { lex FROM }
    $w $h $e $r $e { lex WHERE }
    $i $n $t $o { lex INTO }
    $v $a $l $u $e $s { lex VALUES }
    $s $e $t { lex SET }
    "(" { lex LParen }
    "," { lex Comma }
    ")" { lex RParen }
    "." { lex Dot }
    "*" { lex Mul }
    "/" { lex Div }
    "+" { lex Add }
    "-" { lex Sub }
    "^" { lex Exponent }
    $i $s { lex IS }
    $n $o $t { lex NOT }
    $n $u $l $l { lex NULL_P }
    $i $s $n $u $l $l { lex ISNULL }
    $n $o $t $n $u $l $l { lex NOTNULL }
    "<>" { lex NotEquals }
    "!=" { lex NotEquals }
    "<" { lex LT }
    "<=" { lex LTE }
    ">" { lex GT }
    ">=" { lex GTE }
    "=" { lex Equals }
    $l $i $k $e { lex LIKE }
    $i $l $i $k $e { lex ILIKE }
    $a $n $d { lex AND }
    $o $r { lex OR }
    [\'] ("''" | $quoted)* [\'] { lex' (String . T.pack . unquoteString) }
    $firstLetter $unicodeIds* { lex' (Name . T.pack) }
    -- FIXME integer literal
    "-"? $digit+ ("." $digit+)? ($e "-"? $digit+)? { lex' (Number . read) }
    "$" $digit+ { lex' (NumberedParam . read . tail) }
    "${" $haskell+ "}" { lex' (HaskellParam . T.pack . init . drop 2) }
    ";" { lex Semicolon }

{

data LocToken = LocToken
     { loc :: AlexPosn
     , unLoc :: Token
     } deriving Show

-- commented out PascallCase, where I've added CAPS
data Token = -- Delete | Select | Insert | Update
    -- | Asc | Desc | Order | By | Using | Operator
    Nulls | First | Last
    -- | All | Distinct | On | As
    -- | Union | Except
    -- | From | Where | Into | Values | Set
    -- TODO rename Name -> Ident to match bison
    | Name Text | String Text | Number Double
    | NumberedParam Word | HaskellParam Text
    | LParen | RParen | Comma
    | Mul | Div | Add | Sub | Mod | Exponent
    | Equals | NotEquals | LT | LTE | GT | GTE
    -- | Like | ILike
    -- | And | Or | Not
    | Dot | Semicolon | EOF
    -- all the keywords, from bison
    | ABORT_P | AUTHORIZATION | BETWEEN | ABSOLUTE_P | ACCESS | ACTION | ADD_P
    | ADMIN | AFTER | AGGREGATE | ALL | ALSO | ALTER | ALWAYS | ANALYSE | ANALYZE | AND
    | ANY | ARRAY | AS | ASC | ASSERTION | ASSIGNMENT | ASYMMETRIC | AT | ATTACH
    | ATTRIBUTE | BACKWARD | BEFORE | BEGIN_P | BIGINT | BINARY | BIT | BOOLEAN_P
    | BOTH | BY | CACHE | CALL | CALLED | CASCADE | CASCADED | CASE | CAST | CATALOG_P
    | CHAIN | CHARACTER | CHARACTERISTICS | CHAR_P | CHECK | CHECKPOINT | CLASS
    | CLOSE | CLUSTER | COALESCE | COLLATE | COLLATION | COLUMN | COLUMNS | COMMENT
    | COMMENTS | COMMIT | COMMITTED | CONCURRENTLY | CONFIGURATION | CONFLICT
    | CONNECTION | CONSTRAINT | CONSTRAINTS | CONTENT_P | CONTINUE_P
    | CONVERSION_P | COPY | COST | CREATE | CROSS | CSV | CUBE | CURRENT_CATALOG
    | CURRENT_DATE | CURRENT_P | CURRENT_ROLE | CURRENT_SCHEMA | CURRENT_TIME
    | CURRENT_TIMESTAMP | CURRENT_USER | CURSOR | CYCLE | DATABASE | DATA_P
    | DAY_P | DEALLOCATE | DEC | DECIMAL_P | DECLARE | DEFAULT | DEFAULTS
    | DEFERRABLE | DEFERRED | DEFINER | DELETE_P | DELIMITER | DELIMITERS
    | DEPENDS | DESC | DETACH | DICTIONARY | DISABLE_P | DISCARD | DISTINCT | DO
    | DOCUMENT_P | DOMAIN_P | DOUBLE_P | DROP | EACH | ELSE | ENABLE_P | ENCODING
    | ENCRYPTED | END_P | ENUM_P | ESCAPE | EVENT | EXCEPT | EXCLUDE | EXCLUDING
    | EXCLUSIVE | EXECUTE | EXISTS | EXPLAIN | EXTENSION | EXTERNAL | EXTRACT
    | FALSE_P | FAMILY | FETCH | FILTER | FIRST_P | FLOAT_P | FOLLOWING | FOR | FORCE
    | FOREIGN | FORWARD | FREEZE | FROM | FULL | FUNCTION | FUNCTIONS | GENERATED
    | GLOBAL | GRANT | GRANTED | GREATEST | GROUPING | GROUPS | GROUP_P | HANDLER
    | HAVING | HEADER_P | HOLD | HOUR_P | IDENTITY_P | IF_P | ILIKE | IMMEDIATE
    | IMMUTABLE | IMPLICIT_P | IMPORT_P | INCLUDE | INCLUDING | INCREMENT | INDEX
    | INDEXES | INHERIT | INHERITS | INITIALLY | INLINE_P | INNER_P | INOUT
    | INPUT_P | INSENSITIVE | INSERT | INSTEAD | INTEGER | INTERSECT | INTERVAL
    | INTO | INT_P | INVOKER | IN_P | IS | ISNULL | ISOLATION | JOIN | KEY | LABEL
    | LANGUAGE | LARGE_P | LAST_P | LATERAL_P | LEADING | LEAKPROOF | LEAST | LEFT
    | LEVEL | LIKE | LIMIT | LISTEN | LOAD | LOCAL | LOCALTIME | LOCALTIMESTAMP
    | LOCATION | LOCKED | LOCK_P | LOGGED | MAPPING | MATCH | MATERIALIZED
    | MAXVALUE | METHOD | MINUTE_P | MINVALUE | MODE | MONTH_P | MOVE | NAMES
    | NAME_P | NATIONAL | NATURAL | NCHAR | NEW | NEXT | NO | NONE | NOT | NOTHING
    | NOTIFY | NOTNULL | NOWAIT | NULLIF | NULLS_P | NULL_P | NUMERIC | OBJECT_P | OF
    | OFF | OFFSET | OIDS | OLD | ON | ONLY | OPERATOR | OPTION | OPTIONS | OR | ORDER
    | ORDINALITY | OTHERS | OUTER_P | OUT_P | OVER | OVERLAPS | OVERLAY
    | OVERRIDING | OWNED | OWNER | PARALLEL | PARSER | PARTIAL | PARTITION
    | PASSING | PASSWORD | PLACING | PLANS | POLICY | POSITION | PRECEDING
    | PRECISION | PREPARE | PREPARED | PRESERVE | PRIMARY | PRIOR | PRIVILEGES
    | PROCEDURAL | PROCEDURE | PROCEDURES | PROGRAM | PUBLICATION | QUOTE | RANGE
    | READ | REAL | REASSIGN | RECHECK | RECURSIVE | REF | REFERENCES | REFERENCING
    | REFRESH | REINDEX | RELATIVE_P | RELEASE | RENAME | REPEATABLE | REPLACE
    | REPLICA | RESET | RESTART | RESTRICT | RETURNING | RETURNS | REVOKE | RIGHT
    | ROLE | ROLLBACK | ROLLUP | ROUTINE | ROUTINES | ROW | ROWS | RULE | SAVEPOINT
    | SCHEMA | SCHEMAS | SCROLL | SEARCH | SECOND_P | SECURITY | SELECT | SEQUENCE
    | SEQUENCES | SERIALIZABLE | SERVER | SESSION | SESSION_USER | SET | SETOF
    | SETS | SHARE | SHOW | SIMILAR | SIMPLE | SKIP | SMALLINT | SNAPSHOT | SOME
    | SQL_P | STABLE | STANDALONE_P | START | STATEMENT | STATISTICS | STDIN
    | STDOUT | STORAGE | STORED | STRICT_P | STRIP_P | SUBSCRIPTION | SUBSTRING
    | SUPPORT | SYMMETRIC | SYSID | SYSTEM_P | TABLE | TABLES | TABLESAMPLE
    | TABLESPACE | TEMP | TEMPLATE | TEMPORARY | TEXT_P | THEN | TIES | TIME
    | TIMESTAMP | TO | TRAILING | TRANSACTION | TRANSFORM | TREAT | TRIGGER | TRIM
    | TRUE_P | TRUNCATE | TRUSTED | TYPES_P | TYPE_P | UNBOUNDED | UNCOMMITTED
    | UNENCRYPTED | UNION | UNIQUE | UNKNOWN | UNLISTEN | UNLOGGED | UNTIL | UPDATE
    | USER | USING | VACUUM | VALID | VALIDATE | VALIDATOR | VALUES | VALUE_P
    | VARCHAR | VARIADIC | VARYING | VERBOSE | VERSION_P | VIEW | VIEWS
    | VOLATILE | WHEN | WHERE | WHITESPACE_P | WINDOW | WITH | WITHIN
    | WITHOUT | WORK | WRAPPER | WRITE | XMLATTRIBUTES | XMLCONCAT
    | XMLELEMENT | XMLEXISTS | XMLFOREST | XMLNAMESPACES | XMLPARSE
    | XMLPI | XMLROOT | XMLSERIALIZE | XMLTABLE | XML_P | YEAR_P
    | YES_P | ZONE

     deriving (Show, Read, Eq, Ord)

/* from https://github.com/dagit/happy-plus-alex/blob/master/src/Lexer.x */

-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = filePath <$> alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

-- For nice parser error messages.
unLex :: Token -> String
unLex t = case t of
    Name n -> T.unpack n 
    String s -> T.unpack s
    Number n -> show n
    NumberedParam i -> '$' : show i
    HaskellParam s -> "${" ++ T.unpack s ++ "}"
    LParen  -> "("
    RParen  -> ")"
    Comma -> ","
    Mul -> "*"
    Div -> "/"
    Add -> "+"
    Sub -> "-"
    Exponent -> "^"
    ISNULL -> "IsNull"
    NOTNULL -> "NotNull"
    Equals -> "="
    NotEquals -> "!="
    Semicolon -> ";"
    EOF -> "<EOF>"
    _ -> show t

-- | remove single quotes, and '' escape sequences
unquoteString :: String -> String
unquoteString ('\'' : rest) = go rest
  where
    go  ('\'' : '\'' : rest) = '\'' : go rest
    go ['\''] = ""
    go [_] = error "string did not end with a '"
    go ('\'' : _rest) = error "unescaped ' in middle"
    go (c : rest)  = c : go rest
unquoteString _ = error "string did not begin with a '"

alexEOF :: Alex LocToken
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ LocToken p EOF

-- Unfortunately, we have to extract the matching bit of string ourselves...
lex' :: (String -> Token) -> AlexAction LocToken
lex' f = \(p,_,_,s) i -> return $ LocToken p (f (take i s))

-- For constructing tokens that do not depend on the input
lex :: Token -> AlexAction LocToken
lex = lex' . const

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex LocToken
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexErrorPosn p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexErrorPosn :: AlexPosn -> String -> Alex a
alexErrorPosn (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlexWithFilepath :: Alex a -> FilePath -> String -> Either String a
runAlexWithFilepath a fp input = runAlex input (setFilePath fp >> a)

lexAll :: Alex [LocToken]
lexAll = do
    token <- alexMonadScan
    case unLoc token of
        EOF -> return [token]
        _ -> fmap (token :) lexAll

testLex' :: String -> Either String [LocToken]
testLex' s = runAlex s lexAll

testLex :: String -> Either String [Token]
testLex s = map unLoc <$> testLex' s

}
