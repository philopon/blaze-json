module Text.Blaze.JSON
    ( JSON
      -- * configuration
    , EncodeConfig(..)
    , def

      -- * convert
    , toBuilder
    , toLazyText
    , encode

      -- * constructors
      -- ** null
    , null
      -- ** bool
    , bool
      -- ** number
    , integral, realFloat
      -- ** string
    , text, lazyText, string
      -- ** array
    , array
      -- ** object
    , object

      -- * reexports
    , module Text.Blaze.JSON.Class
    ) where

import Prelude hiding (null)

import Text.Blaze.JSON.Internal
import Text.Blaze.JSON.Class
import Data.Default.Class

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L

import qualified Data.Text.Lazy.Builder as B

import qualified Data.ByteString.Lazy as L

-- | convert JSON to lazy-Text
toLazyText :: EncodeConfig -> JSON -> L.Text
toLazyText cfg = B.toLazyText . toBuilder cfg

-- | encode to Aeson compat bytesting
encode :: JSON -> L.ByteString
encode = L.encodeUtf8 . toLazyText def
