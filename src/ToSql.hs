{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ToSql where

import           Data.Functor.Contravariant
import           Data.Functor.Contravariant.Divisible
import           Data.ByteString (ByteString)

import qualified Data.ByteString as BS
import qualified Database.PostgreSQL.LibPQ as PQ

data FieldEncoder a = FieldEncoder PQ.Oid (a -> ByteString)

instance Contravariant FieldEncoder where
    contramap f (FieldEncoder oid enc) = FieldEncoder oid (enc . f)

-- TODO benchmark, consider not-list
newtype SqlEncoder a = SqlEncoder [FieldEncoder a]

instance Contravariant SqlEncoder where
    contramap f (SqlEncoder fields) = SqlEncoder (map (contramap f) fields)

instance Divisible SqlEncoder where
    divide pair fb fc = SqlEncoder (bs <> cs) where
      SqlEncoder bs = contramap (fst . pair) fb
      SqlEncoder cs = contramap (snd . pair) fc
    conquer = SqlEncoder [] -- identity, does not actually encode any fields

class ToSql a where
    toSql :: SqlEncoder a

instance (ToSql a, ToSql b) => ToSql (a, b) where
    toSql = divided toSql toSql
instance (ToSql a, ToSql b, ToSql c) => ToSql (a, b, c) where
    toSql = divide (\(a, b, c) -> (a, (b, c))) toSql toSql
instance (ToSql a, ToSql b, ToSql c, ToSql d) => ToSql (a, b, c, d) where
    toSql = divide (\(a, b, c, d) -> (a, (b, c, d))) toSql toSql
instance (ToSql a, ToSql b, ToSql c, ToSql d, ToSql e) => ToSql (a, b, c, d, e) where
    toSql = divide (\(a, b, c, d, e) -> (a, (b, c, d, e))) toSql toSql
instance (ToSql a, ToSql b, ToSql c, ToSql d, ToSql e, ToSql f) => ToSql (a, b, c, d, e, f) where
    toSql = divide (\(a, b, c, d, e, f) -> (a, (b, c, d, e, f))) toSql toSql
instance (ToSql a, ToSql b, ToSql c, ToSql d, ToSql e, ToSql f, ToSql g) => ToSql (a, b, c, d, e, f, g) where
    toSql = divide (\(a, b, c, d, e, f, g) -> (a, (b, c, d, e, f, g))) toSql toSql
instance (ToSql a, ToSql b, ToSql c, ToSql d, ToSql e, ToSql f, ToSql g, ToSql h) => ToSql (a, b, c, d, e, f, g, h) where
    toSql = divide (\(a, b, c, d, e, f, g, h) -> (a, (b, c, d, e, f, g, h))) toSql toSql
instance (ToSql a, ToSql b, ToSql c, ToSql d, ToSql e, ToSql f, ToSql g, ToSql h, ToSql i) => ToSql (a, b, c, d, e, f, g, h, i) where
    toSql = divide (\(a, b, c, d, e, f, g, h, i) -> (a, (b, c, d, e, f, g, h, i))) toSql toSql
instance (ToSql a, ToSql b, ToSql c, ToSql d, ToSql e, ToSql f, ToSql g, ToSql h, ToSql i, ToSql j) => ToSql (a, b, c, d, e, f, g, h, i, j) where
    toSql = divide (\(a, b, c, d, e, f, g, h, i, j) -> (a, (b, c, d, e, f, g, h, i, j))) toSql toSql
instance (ToSql a, ToSql b, ToSql c, ToSql d, ToSql e, ToSql f, ToSql g, ToSql h, ToSql i, ToSql j, ToSql k) => ToSql (a, b, c, d, e, f, g, h, i, j, k) where
    toSql = divide (\(a, b, c, d, e, f, g, h, i, j, k) -> (a, (b, c, d, e, f, g, h, i, j, k))) toSql toSql
