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
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

lit :: Gen Literal
lit = Gen.choice
  [ I <$> Gen.integral (Range.linearFrom 0 minBound maxBound)
  , F <$> Gen.double (Range.linearFracFrom 0 (-1e300) 1e300)
  , T <$> Gen.text (Range.linear 0 100) unicodeNotNull
  , B <$> Gen.bool
  , pure Null
  ]

litE :: Gen Expr
litE = Lit <$> lit

unicodeNotNull :: Gen Char
unicodeNotNull = Gen.filter (\c -> c /= '\0' && c /= '\'' ) Gen.unicode

name_ :: Gen Name
name_ = Name <$> Gen.filter (flip Set.notMember keywords)
  (T.cons <$> Gen.lower <*>
    Gen.text (Range.linear 0 29) (Gen.frequency [(26, Gen.lower), (1, pure '_')]))

keywords :: Set Text
keywords = Set.fromList
  [ "and", "delete", "from", "ilike", "insert", "into", "is", "isnull", "like" , "not", "notnull"
  , "null", "or", "select", "values", "where" ]

expr :: Gen Expr
expr = normalizeExpr <$> Gen.choice -- TODO frequency
  [ litE
  , CRef <$> name_
  , NumberedParam <$> Gen.integral (Range.linear 1 20)
  , HaskellParam <$> Gen.text (Range.linear 1 30) Gen.alphaNum -- TODO valid Haskell variable names
  , Unary <$> unaryOp <*> scaleOne expr
  , BinOp <$> binOp <*> scaleHalf expr <*> scaleHalf expr
  , Indirection <$> scaleOne expr <*> Gen.nonEmpty (Range.linear 1 4) name_
  -- TODO remaining constructors
  ]

-- TODO recursion scheme for Expr, remove common code of maxParamExpr, this
-- | Some combinations don't make sense.  Handle them here if we can
-- consolodate (not 'discard').
normalizeExpr :: Expr -> Expr
normalizeExpr = go
  where go = \case
          -- normalize the i nnermost NegateNum, not recursively
          Unary Negate (Lit (I x)) | x > 0 -> Lit (I (-1 * x))
          Unary Negate (Lit (F x)) | x > 0 -> Lit (F (-1 * x))
          Unary o inner -> Unary o (go inner)
          BinOp o l r -> BinOp o (go l) (go r)
          Indirection e is -> Indirection (go e) is
          And l r -> And (go l) (go r)
          Or l r -> Or (go l) (go r)
          L LikeE {..} -> L LikeE
            { op
            , string = go string
            , likePattern = go likePattern
            , escape = go <$> escape
            , invert = invert
            }
          -- TODO SelectExpr
          expression -> expression

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
