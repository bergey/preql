{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | taken verbatim from Database.PostgreSQL.Simple.Internal.PQResultUtils

module Preql.PQResultUtils where

import           Control.Exception                    as E
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as B
import           Data.Foldable                        (for_)
import qualified Data.Vector                          as V
import qualified Data.Vector.Mutable                  as MV
import qualified Data.Vector.Unboxed                  as VU
import qualified Data.Vector.Unboxed.Mutable          as MVU
import qualified Database.PostgreSQL.LibPQ            as PQ
import           Database.PostgreSQL.Simple.FromField (ResultError (..))
import           Database.PostgreSQL.Simple.Internal  as Base hiding (result,
                                                               row)
import           Database.PostgreSQL.Simple.Ok
import           Database.PostgreSQL.Simple.TypeInfo
import           Database.PostgreSQL.Simple.Types     (Query (..))

finishQueryWith :: RowParser r -> Connection -> Query -> PQ.Result -> IO [r]
finishQueryWith parser conn q result = finishQueryWith' q result $ do
    nrows <- PQ.ntuples result
    ncols <- PQ.nfields result
    forM' 0 (nrows-1) $ \row ->
        getRowWith parser row ncols conn result

finishQueryWithV :: RowParser r -> Connection -> Query -> PQ.Result -> IO (V.Vector r)
finishQueryWithV parser conn q result = finishQueryWith' q result $ do
    nrows <- PQ.ntuples result
    let PQ.Row nrows' = nrows
    ncols <- PQ.nfields result
    mv <- MV.unsafeNew (fromIntegral nrows')
    for_ [ 0 .. nrows-1 ] $ \row -> do
        let PQ.Row row' = row
        value <- getRowWith parser row ncols conn result
        MV.unsafeWrite mv (fromIntegral row') value
    V.unsafeFreeze mv

finishQueryWithVU :: VU.Unbox r => RowParser r -> Connection -> Query -> PQ.Result -> IO (VU.Vector r)
finishQueryWithVU parser conn q result = finishQueryWith' q result $ do
    nrows <- PQ.ntuples result
    let PQ.Row nrows' = nrows
    ncols <- PQ.nfields result
    mv <- MVU.unsafeNew (fromIntegral nrows')
    for_ [ 0 .. nrows-1 ] $ \row -> do
        let PQ.Row row' = row
        value <- getRowWith parser row ncols conn result
        MVU.unsafeWrite mv (fromIntegral row') value
    VU.unsafeFreeze mv

finishQueryWith' :: Query -> PQ.Result -> IO a -> IO a
finishQueryWith' q result k = do
  status <- PQ.resultStatus result
  case status of
    PQ.TuplesOk -> k
    PQ.EmptyQuery    -> queryErr "query: Empty query"
    PQ.CommandOk     -> queryErr "query resulted in a command response (did you mean to use `execute` or forget a RETURNING?)"
    PQ.CopyOut       -> queryErr "query: COPY TO is not supported"
    PQ.CopyIn        -> queryErr "query: COPY FROM is not supported"
#if MIN_VERSION_postgresql_libpq(0,9,3)
    PQ.CopyBoth      -> queryErr "query: COPY BOTH is not supported"
#endif
#if MIN_VERSION_postgresql_libpq(0,9,2)
    PQ.SingleTuple   -> queryErr "query: single-row mode is not supported"
#endif
    PQ.BadResponse   -> throwResultError "query" result status
    PQ.NonfatalError -> throwResultError "query" result status
    PQ.FatalError    -> throwResultError "query" result status
  where
    queryErr msg = throwIO $ QueryError msg q

getRowWith :: RowParser r -> PQ.Row -> PQ.Column -> Connection -> PQ.Result -> IO r
getRowWith parser row ncols conn result = do
  let rw = Row row result
  let unCol (PQ.Col x) = fromIntegral x :: Int
  okvc <- runConversion (runStateT (runReaderT (unRP parser) rw) 0) conn
  case okvc of
    Ok (val,col) | col == ncols -> return val
                 | otherwise -> do
                     vals <- forM' 0 (ncols-1) $ \c -> do
                         tinfo <- getTypeInfo conn =<< PQ.ftype result c
                         v <- PQ.getvalue result row c
                         return ( tinfo
                                , fmap ellipsis v       )
                     throw (ConversionFailed
                      (show (unCol ncols) ++ " values: " ++ show vals)
                      Nothing
                      ""
                      (show (unCol col) ++ " slots in target type")
                      "mismatch between number of columns to convert and number in target type")
    Errors []  -> throwIO $ ConversionFailed "" Nothing "" "" "unknown error"
    Errors [x] -> throwIO x
    Errors xs  -> throwIO $ ManyErrors xs

ellipsis :: ByteString -> ByteString
ellipsis bs
    | B.length bs > 15 = B.take 10 bs `B.append` "[...]"
    | otherwise        = bs

forM' :: (Ord n, Num n) => n -> n -> (n -> IO a) -> IO [a]
forM' lo hi m = loop hi []
  where
    loop !n !as
      | n < lo = return as
      | otherwise = do
           a <- m n
           loop (n-1) (a:as)
{-# INLINE forM' #-}
