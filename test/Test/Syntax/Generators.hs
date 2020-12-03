{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Syntax.Generators where

import Preql.QuasiQuoter.Syntax.Name
import Preql.QuasiQuoter.Syntax.Syntax as Syntax

import Control.Applicative
import Control.Monad
import Data.Set (Set)
import Data.Text (Text)
import Hedgehog
import Hedgehog.Internal.Range (clamp)
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

lit :: Gen Literal
lit = Gen.choice
  -- positive numeric literals only, use Unary Negate for negative
  [ I <$> Gen.integral (Range.linearFrom 1 0 maxBound)
  , F <$> Gen.double (Range.linearFracFrom 1 0 1e300)
  , T <$> Gen.text (Range.linear 0 100) unicodeNotControl
  , B <$> Gen.bool
  , pure Null
  ]

litE :: Gen Expr
litE = Lit <$> lit

unicodeNotControl :: Gen Char
unicodeNotControl = Gen.filter (\c -> Char.ord c > 31 && c /= '\DEL' && c /= '\'' ) Gen.unicode

name_ :: Gen Name
name_ = Name <$> Gen.filter (flip Set.notMember keywords)
  (T.cons <$> Gen.lower <*>
    Gen.text (Range.linear 0 29) (Gen.frequency [(26, Gen.lower), (1, pure '_')]))

haskellVarName :: Gen Text
haskellVarName = T.cons <$> Gen.lower <*> Gen.text (Range.linear 0 29) Gen.alphaNum

select :: Gen SelectStmt
select = Gen.sized \size -> do
  nTables <- Gen.integral (Range.linear 1 10)
  let scale = Gen.scale (clampSize . (`div` Size nTables))
  from <- Gen.list (Range.singleton nTables) (scale tableRef)
  return $ Simple Syntax.select {from}

expr :: Gen Expr
expr = Gen.sized \case
  -- TODO frequency
  0 -> Gen.choice zeros
  1 -> Gen.choice (zeros ++ ones)
  n -> Gen.choice (zeros ++ ones ++ twos)
 where
  zeros =
    [ litE
    , CRef <$> name_
    , NumberedParam <$> Gen.integral (Range.linear 1 20)
    , HaskellParam <$> haskellVarName
    ]
  ones =
    [ Unary <$> unaryOp <*> scaleOne expr
    , L <$> scaleOne likeE
    ]
  twos =
    [ BinOp <$> binOp <*> scaleHalf expr <*> scaleHalf expr
    , Indirection <$> scaleOne expr <*> Gen.nonEmpty (Range.linear 1 4) name_
    -- TODO SelectExpr
    -- TODO FunctionApplication
    , Cas <$> caseE
    ]

unaryOp :: Gen UnaryOp
unaryOp = Gen.enumBounded

binOp :: Gen BinOp
binOp = Gen.enumBounded

likeOp :: Gen LikeOp
likeOp = Gen.enumBounded

likeE :: Gen LikeE
likeE = do
  op <- likeOp
  string <- scaleHalf expr
  likePattern <- scaleHalf expr
  escape <- Gen.frequency
    [(50, pure Nothing), (49, Just . Lit . T . T.singleton <$> Gen.alphaNum), (1, Just <$> scaleHalf expr)]
  invert <- Gen.bool
  return LikeE {..}

caseE :: Gen Case
caseE = do
  hasImplicit <- Gen.bool
  hasElse <- Gen.bool
  whenSize <- Gen.integral (Range.linear 1 10)
  let
    oneIf x = if x then 1 else 0
    n = 2 * whenSize + oneIf hasImplicit + oneIf hasElse
    scale = Gen.scale (clampSize . (`div` n))
    justIf x = if x then Just <$> scale expr else pure Nothing
  whenClause <- replicateM (Range.unSize whenSize) (liftA2 (,) (scale expr) (scale expr))
  implicitArg <- justIf hasImplicit
  elseClause <- justIf hasElse
  return Case{..}

tableRef :: Gen TableRef
tableRef = Gen.sized \case
    0 -> singleTable
    1 -> Gen.choice [ singleTable, aliased ]
    n -> Gen.choice [ singleTable, aliased, subSelect ]
  where
    singleTable = (J <$> joinedTable)
    aliased = As <$> scaleOne joinedTable <*> alias
    alias = Alias <$> name_ <*>
      Gen.choice [pure [], Gen.list (Range.linear 1 5) name_]
    subSelect = SubSelect <$> scaleOne Test.Syntax.Generators.select <*> alias

joinedTable :: Gen JoinedTable
joinedTable = Gen.sized \case
    0 -> singleTable
    1 -> singleTable
    n -> Gen.choice [ singleTable, joined, crossJoin ]
  where
    singleTable = Table <$> name_
    joined = Join <$> Gen.enumBounded <*> joinQual
             <*> scaleHalf tableRef <*> scaleHalf tableRef
    crossJoin = CrossJoin <$> scaleHalf tableRef <*> scaleHalf tableRef

joinQual :: Gen JoinQual
joinQual = Gen.choice
  [ pure Natural
  , Using <$> Gen.list (Range.linear 1 10) name_
  , On <$> scaleOne expr
  ]

clampSize :: Size -> Size
clampSize = clamp 0 99

scaleOne :: MonadGen m => m a -> m a
scaleOne = Gen.scale (\s -> clampSize (s - 1))

scaleHalf :: MonadGen m => m a -> m a
scaleHalf = Gen.scale (clampSize . (`div` 2))

keywords :: Set Text
keywords = Set.union reserved_keywords type_func_name_keyword

-- Reserved keyword - these keywords are usable only as a ColLabel.
reserved_keywords :: Set Text
reserved_keywords = Set.fromList
  [ "all", "analyse", "analyze", "and", "any", "array", "as", "asc", "asymmetric", "both", "case", "cast", "check", "collate", "column", "constraint", "create", "current_catalog", "current_date", "current_role", "current_time", "current_timestamp", "current_user", "default", "deferrable", "desc", "distinct", "do", "else", "end", "except", "false", "fetch", "for", "foreign", "from", "grant", "group", "having", "in", "initially", "intersect", "into", "lateral", "leading", "limit", "localtime", "localtimestamp", "not", "null", "offset", "on", "only", "or", "order", "placing", "primary", "references", "returning", "select", "current_user", "some", "symmetric", "table", "then", "to", "trailing", "true", "union", "unique", "user", "using", "variadic", "when", "where", "window", "with" ]

-- Type/function identifier - keywords that can be type or function names.
type_func_name_keyword :: Set Text
type_func_name_keyword = Set.fromList
  [ "authorization", "binary", "collation", "concurrently", "cross", "current_schema", "freeze", "full", "ilike", "inner", "is", "isnull", "join", "left", "like", "natural", "notnull", "outer", "overlaps", "right", "similar", "tablesample", "verbose" ]
