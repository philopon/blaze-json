module Text.Blaze.JSON
    ( JSON
      -- * configuration
    , EncodeConfig(..)
    , def

      -- * convert
    , toBuilder
    , encodeWith
    , encode

      -- * constructors
      -- ** null
    , null
      -- ** bool
    , bool
      -- ** number
    , integral, double
      -- ** string
    , text, lazyText
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
