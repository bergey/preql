{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Description: newtype to support FromSql instances for row types.

module Preql.FromSql.Tuple where

import Preql.FromSql.Class (FieldDecoder(..))
import Preql.Wire.Errors (PgType(..))

import Control.Monad (unless)
import Data.Bits ((.|.), Bits, shiftL)
import Data.Int (Int32)
import qualified BinaryParser as BP
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Database.PostgreSQL.LibPQ as PQ

-- | Wrapper for Postgres anonymous row types (sometimes called record
-- types), so instance resolution picks the right decoder.  The useful
-- instances are for (Haskell) tuples.  Postgres allows row types with
-- a single field, but the instances would overlap with those for
-- nested row types, so we do not provide them.
newtype Tuple r = Tuple r
  deriving (Eq, Ord, Show)

-- Unlike the same-named functions in PostgreSQL.Binary.Decoding, these check
-- the number of components and the OIDs of each component.

-- | Helper for decoding composites
newtype Composite a = Composite (BP.BinaryParser a)
  deriving newtype (Functor, Applicative)

composite :: Int -> Composite a -> BP.BinaryParser a
composite n (Composite parser) = do
  size <- intOfSize 4
  unless (size == n) (BP.failure "composite has wrong size")
  parser

valueComposite :: FieldDecoder a -> Composite a
valueComposite (FieldDecoder pgType parser) = Composite $ do
  case pgType of
    -- For now, we only confirm statically-known OIDs.  To do more, we'll need
    -- to feed through the OID cache (not yet implemented) and the Connection
    -- (which will be a problem if we ever implement lazy decoding)
    -- Statically-known OIDs seems like a reasonable compromise for now, until
    -- other parts of the design stabilize.
    Oid (PQ.Oid expected) _ -> do
      actual <- intOfSize 4
      unless (actual == expected) (BP.failure $ "OID in composite expected=" <> showt expected <> " actual=" <> showt actual)
    TypeName _ -> BP.unitOfSize 4 -- TODO check cache, maybe query DB
  onContent parser >>=
    maybe (BP.failure "unexpected null") pure
  where showt = T.pack . show

{-# INLINE intOfSize #-}
intOfSize :: (Integral a, Bits a) => Int -> BP.BinaryParser a
intOfSize x =
  fmap (BS.foldl' (\n h -> shiftL n 8 .|. fromIntegral h) 0) (BP.bytesOfSize x)

{-# INLINABLE onContent #-}
onContent :: BP.BinaryParser a -> BP.BinaryParser ( Maybe a )
onContent decoder = do
  size :: Int32 <- intOfSize 4
  case size of
    (-1) -> pure Nothing
    n -> fmap Just (BP.sized (fromIntegral n) decoder)
