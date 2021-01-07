{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Syntax.Generators where

import Preql.QuasiQuoter.Syntax.Name
import Preql.QuasiQuoter.Syntax.Syntax as Syntax

import Control.Applicative
import Control.Monad
import Data.Maybe (isNothing)
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
  [ I <$> Gen.sized \case
      -- randomly pick [1,10] when we get down to size 1
      1 -> Gen.prune (Gen.integral (Range.linear 1 10))
      _ -> Gen.integral (Range.linearFrom 1 0 maxBound)
  , F <$> Gen.double (Range.linearFracFrom 1 0 1e300)
  , T <$> Gen.text (Range.linear 0 100) unicodeNotControl
  , B <$> Gen.bool
  , pure Null
  ]

litE :: Gen Expr
litE = Lit <$> lit

unicodeNotControl :: Gen Char
unicodeNotControl = Gen.filter (\c -> Char.ord c > 31 && c /= '\DEL' && c /= '\'' ) Gen.unicode

-- TODO Gen.prune once we get down to length 1
name_ :: Gen Name
name_ = Gen.sized \case
  -- don't shrink to "a"; assume all letters are the same, make queries more legible
  1 -> Gen.prune (Name <$> (T.cons <$> Gen.lower <*> pure ""))
  _ -> Name <$> Gen.filter (flip Set.notMember keywords)
    (T.cons <$> Gen.lower <*>
     Gen.text (Range.linear 0 29) (Gen.frequency [(26, Gen.lower), (1, pure '_')]))

haskellVarName :: Gen Text
haskellVarName = T.cons <$> Gen.lower <*> Gen.text (Range.linear 0 29) Gen.alphaNum


select :: Gen SelectStmt
select = select_

select_ :: Gen SelectStmt
select_ = Gen.sized \case
  1 -> Gen.frequency smallSelects
  _ -> Gen.frequency ( smallSelects ++ [ (20, setSelect) ])
 where
  smallSelects =
    [ (40, Simple <$> simpleSelect)
    , (20, SelectValues <$> Gen.nonEmpty (Range.exponential 1 100)
            (Gen.nonEmpty (Range.exponential 1 20) (scaleOne valueExpr)))
    , (20, S <$> selectWithoutOptions <*> selectOptions_)
    ]
  setSelect = Set <$> Gen.enumBounded <*> Gen.enumBounded <*> scaleHalf select_ <*> scaleHalf select_
  selectWithoutOptions = Gen.filter noOptions (scaleOne select_)
  noOptions (S _ _) = False
  noOptions _ = True

scaledList :: Range Int -> Gen a -> Gen [a]
scaledList range ga = do
  n <- Gen.integral range
  let scale = Gen.scale (clampSize . (`div` Size n))
  Gen.list (Range.singleton n) (scale ga)

simpleSelect :: Gen Select
simpleSelect = do
  -- now bind fields of Syntax.Select
  distinct <- Gen.maybe distinctClause
  -- TODO why don't we support table.* in ResTarget?  Is it part of a_expr?
  targetList <- Gen.frequency [(1, pure [Star]), (99, scaledList (Range.linear 1 15) columnTarget)]
  from <- scaledList (Range.linear 1 10) tableRef
  whereClause <- Gen.maybe (scaleHalf expr)
  groupBy <- scaledList (Range.linear 0 5) expr
  having <- Gen.maybe (scaleHalf expr)
  return $ Syntax.select {distinct, from, targetList, whereClause, groupBy, having}

selectOptions_ :: Gen SelectOptions
selectOptions_ = Gen.filter nonTrivial do
  sortBy <- Gen.list (Range.linear 0 5) sortBy_
  offset <- Gen.maybe (scaleOne expr)
  limit <- Gen.maybe (scaleOne expr)
  locking <- scaledList (Range.linear 0 3) locking_
  let withClause = Nothing
  return SelectOptions{..}
 where
   nonTrivial SelectOptions{..} =
     not (null sortBy && isNothing offset && isNothing limit && null locking && isNothing withClause)

valueExpr :: Gen Expr
valueExpr = Gen.filter notSelectExpr expr where
  notSelectExpr (SelectExpr _) = False
  notSelectExpr _ = True

expr :: Gen Expr
expr = Gen.sized \case
  -- TODO frequency
  0 -> Gen.choice zeros
  1 -> Gen.choice (zeros ++ ones)
  _ -> Gen.choice (zeros ++ ones ++ twos)
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
    , Indirection <$> scaleOne expr <*> Gen.nonEmpty (Range.linear 1 4) name_
    ]
  twos =
    [ BinOp <$> binOp <*> scaleHalf expr <*> scaleHalf expr
    -- One might expect scaleOne for SelectExpr, but in practice that
    -- leads to unacceptably long test times for Expr.  It might be
    -- nice to work out the maximum number of constructors (or p90?)
    -- in SelectStmt of size n, and do something more precise here.
    , SelectExpr <$> scaleHalf select_
    , Fun <$> funApp
    , Cas <$> caseE
    ]

unaryOp :: Gen UnaryOp
unaryOp = Gen.enumBounded

binOp :: Gen BinOp
binOp = Gen.enumBounded

mathOp :: Gen BinOp
mathOp = Gen.element [ Mul, Div, Add, Sub, Exponent, Mod, Eq, Syntax.LT, LTE, Syntax.GT, GTE, NEq ]

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

funApp :: Gen FunctionApplication
funApp = do
  name <- name_
  indirection <- Gen.list (Range.linear 0 3) name_
  len_arguments <- Gen.integral (Range.linear (-1) 10)
  len_sortBy <- if len_arguments > 0 then pure 0 else Gen.integral (Range.linear 0 3)
  len_withinGroup <- if len_sortBy == 0 then pure 0 else Gen.integral (Range.linear 0 3)
  hasFilter <- Gen.bool
  hasOver <- Gen.bool
  let
    n = len_sortBy + len_withinGroup + min 0 len_arguments + if hasFilter then 1 else 0 + if hasOver then 3 else 0
    scale = if n <= 0 then id else Gen.scale (clampSize . (`div` Size n))
  arguments <- case len_arguments of
    -1 -> pure StarArg
    0 -> pure NoArgs
    n -> do
      arguments <- Gen.nonEmpty (Range.singleton len_arguments)
        (Gen.choice [ E <$> scale expr, Named <$> name_ <*> scale expr ])
      sortBy <- Gen.list (Range.singleton len_sortBy) (scale sortBy_)
      distinct <- Gen.bool
      return $ Args ArgsList {..}
  withinGroup <- Gen.list (Range.singleton len_withinGroup) (scale sortBy_)
  filterClause <- if hasFilter then Just <$> scale expr  else pure Nothing
  over <- if hasOver then scale over_ else pure noWindow
  return FApp{..}

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
    _ -> Gen.choice [ singleTable, aliased, subSelect ]
  where
    singleTable = J <$> joinedTable
    aliased = As <$> scaleOne joinedTable <*> alias
    alias = Alias <$> name_ <*>
      Gen.choice [pure [], Gen.list (Range.linear 1 5) name_]
    subSelect = SubSelect <$> scaleOne select_ <*> alias

joinedTable :: Gen JoinedTable
joinedTable = Gen.sized \case
    0 -> singleTable
    1 -> singleTable
    _ -> Gen.choice [ singleTable, joined, crossJoin ]
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

-- | 'Star' is generated in 'select', so we only do @Column@ here
columnTarget :: Gen ResTarget
columnTarget = Column <$> valueExpr <*> Gen.maybe name_

distinctClause :: Gen DistinctClause
distinctClause = Gen.frequency
  [ (1, pure DistinctAll)
  , (9, DistinctOn <$> Gen.nonEmpty (Range.linear 1 5) expr) ]

over_ :: Gen Over
over_ = do
  Gen.choice [ WindowName <$> name_ , Window <$> window_ ]

window_ :: Gen WindowSpec
window_ = do
  refName <- Gen.maybe name_
  partitionClause <- Gen.list (Range.linear 0 3) expr
  orderClause <- Gen.list (Range.linear 0 3) sortBy_
  return WindowSpec {..}

sortBy_ :: Gen SortBy
sortBy_ = SortBy <$> expr <*> sortOrderOrUsing <*> nullsOrder

sortOrderOrUsing :: Gen SortOrderOrUsing
sortOrderOrUsing = Gen.choice [ SortOrder <$> sortOrder, SortUsing <$> mathOp ]

sortOrder :: Gen SortOrder
sortOrder = Gen.enumBounded

nullsOrder :: Gen NullsOrder
nullsOrder = Gen.enumBounded

locking_ :: Gen Locking
locking_ = do
  strength <- Gen.enumBounded
  tables <- Gen.list (Range.linear 0 5) name_
  wait <- Gen.enumBounded
  return Locking{..}

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
