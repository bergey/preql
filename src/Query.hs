-- |

module Query where

import Data.ByteString (ByteString)
import Control.Concurrent.MVar

-- | A @Query@ is a string ready to be passed to Postgres, with
-- phantom type parameters describing its parameters and result.
-- Depending how the @Query@ was constructed, these parameters may be
-- inferred from context (offering no added type safety), or be
-- partly synthesized from the underlying string.
newtype Query params result = Query ByteString

newtype QueryAndParams result = Q (Query params result) params
