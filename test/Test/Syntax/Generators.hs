{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Test.Syntax.Generators where

import Preql.QuasiQuoter.Syntax.Name
import Preql.QuasiQuoter.Syntax.Syntax

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
  [ I <$> Gen.integral (Range.linearFrom 1 0 maxBound)
  , F <$> Gen.double (Range.linearFracFrom 0 (-1e300) 1e300)
  , T <$> Gen.text (Range.linear 0 100) unicodeNotControl
  , B <$> Gen.bool
  , pure Null
  ]

litE :: Gen Expr
litE = Lit <$> lit

unicodeNotControl :: Gen Char
unicodeNotControl = Gen.filter (\c -> Char.ord c > 31 && c /= '\DEL' && c /= '\'' ) Gen.unicode

name_ :: Gen Name
name_ = Name <$> Gen.filter (flip Set.notMember reserved_keywords)
  (T.cons <$> Gen.lower <*>
    Gen.text (Range.linear 0 29) (Gen.frequency [(26, Gen.lower), (1, pure '_')]))

expr :: Gen Expr
expr = Gen.choice -- TODO frequency
  [ litE
  , CRef <$> name_
  , NumberedParam <$> Gen.integral (Range.linear 1 20)
  , HaskellParam <$> Gen.text (Range.linear 1 30) Gen.alphaNum -- TODO valid Haskell variable names
  , Unary <$> unaryOp <*> scaleOne expr
  , BinOp <$> binOp <*> scaleHalf expr <*> scaleHalf expr
  , Indirection <$> scaleOne expr <*> Gen.nonEmpty (Range.linear 1 4) name_
  -- TODO remaining constructors
  ]

unaryOp :: Gen UnaryOp
unaryOp = Gen.enumBounded

binOp :: Gen BinOp
binOp = Gen.enumBounded

clampSize :: Size -> Size
clampSize = clamp 0 99

scaleOne :: MonadGen m => m a -> m a
scaleOne = Gen.scale (\s -> clampSize (s - 1))

scaleHalf :: MonadGen m => m a -> m a
scaleHalf = Gen.scale (clampSize . (`div` 2))

-- Reserved keyword - these keywords are usable only as a ColLabel.
reserved_keywords :: Set Text
reserved_keywords = Set.fromList
  [ "all", "analyse", "analyze", "and", "any", "array", "as", "asc", "asymmetric", "both", "case", "cast", "check", "collate", "column", "constraint", "create", "current_catalog", "current_date", "current_role", "current_time", "current_timestamp", "current_user", "default", "deferrable", "desc", "distinct", "do", "else", "end", "except", "false", "fetch", "for", "foreign", "from", "grant", "group", "having", "in", "initially", "intersect", "into", "lateral", "leading", "limit", "localtime", "localtimestamp", "not", "null", "offset", "on", "only", "or", "order", "placing", "primary", "references", "returning", "select", "current_user", "some", "symmetric", "table", "then", "to", "trailing", "true", "union", "unique", "user", "using", "variadic", "when", "where", "window", "with" ]
