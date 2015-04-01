{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Blaze.JSON.Internal
    ( JSON(..), toBuilder
    , EncodeConfig(..)
    , unsafeToJSON
    , bool
    , null
    , integral
    , realFloat
    , text
    , lazyText
    , string
    , array', array
    , object', object
    , unsafeObject', unsafeObject
    ) where

import Prelude hiding (null)

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Array as A
import qualified Data.Text.Internal.Builder as I
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as BI
import qualified Data.Text.Lazy.Builder.RealFloat as BF

import Data.Default.Class(Default(..))

import Data.Word
import Data.Typeable
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import qualified Data.Foldable as F

import Unsafe.Coerce

-- $setup
-- >>> :set -XOverloadedStrings

-- | JSON encoding data type
newtype JSON = JSON { unJSON :: EncodeConfig -> B.Builder }
    deriving(Typeable)

instance Eq JSON where
    a == b = unJSON a def == unJSON b def

instance Ord JSON where
    a `compare` b = unJSON a def `compare` unJSON b def

-- | convert JSON to Text-builder
toBuilder :: EncodeConfig -> JSON -> B.Builder
toBuilder = flip unJSON
{-# INLINABLE toBuilder #-}

instance Show JSON where
    show = show . flip unJSON def
    {-# INLINABLE show #-}

-- | @
-- def = EncodeConfig
--     { isEscape = const False
--     }
-- @
newtype EncodeConfig = EncodeConfig
    { isEscape :: Char -> Bool -- ^ optional characters which 4hex-digit encoding
    }

instance Default EncodeConfig where
    def = EncodeConfig (const False)
    {-# INLINABLE def #-}

ord :: Char -> Word16
ord = unsafeCoerce . Char.ord

word16 :: Word16 -> B.Builder
word16 w = I.writeN 1 $ \a i -> A.unsafeWrite a i w

word16x2 :: Word16 -> Word16 -> B.Builder
word16x2 u l = I.writeN 2 $ \a i -> do
    A.unsafeWrite a i u
    A.unsafeWrite a (i + 1) l

fourHexDigit :: Word16 -> Word16 -> Word16 -> Word16 -> B.Builder
fourHexDigit d3 d2 d1 d0 = I.writeN 6 $ \a i -> do
    A.unsafeWrite a  i      reverseSolidus
    A.unsafeWrite a (i + 1) (ord 'u')
    A.unsafeWrite a (i + 2) d3
    A.unsafeWrite a (i + 3) d2
    A.unsafeWrite a (i + 4) d1
    A.unsafeWrite a (i + 5) d0

escapeChar :: Char -> B.Builder
escapeChar c0 =
    let (d3, c1) = Char.ord c0 `quotRem` 0x1000
        (d2, c2) = c1 `quotRem` 0x100
        (d1, d0) = c2 `quotRem` 0x10
    in fourHexDigit (digit d3) (digit d2) (digit d1) (digit d0)
  where
    digit = unsafeCoerce . Char.intToDigit

quotationMark :: Word16
quotationMark  = ord '"'

reverseSolidus :: Word16
reverseSolidus = ord '\\'

comma :: Word16
comma = ord ','

escape :: (Char -> Bool) -> Char -> B.Builder
escape check c = Map.findWithDefault noDict c dict
  where
    bs   = word16x2 reverseSolidus
    dict = Map.fromList
        [ ('"',  bs quotationMark)
        , ('\\', bs reverseSolidus)
        , ('\n', bs $ ord 'n')
        , ('\t', bs $ ord 't')
        , ('\r', bs $ ord 'r')
        ]
    noDict = if check c || c < '\x20' then escapeChar c else B.singleton c 

unsafeToJSON :: B.Builder -> JSON
unsafeToJSON = JSON . const
{-# INLINABLE unsafeToJSON #-}

-- | json null value
--
-- >>> null
-- "null"
null :: JSON
null = unsafeToJSON $ B.fromText "null"
{-# INLINABLE null #-}

-- | json boolean value from Bool
--
-- >>> bool True
-- "true"
bool :: Bool -> JSON
bool True  = unsafeToJSON $ B.fromText "true"
bool False = unsafeToJSON $ B.fromText "false"
{-# INLINABLE bool #-}

-- | json number value from Integral
--
-- >>> integral 32
-- "32"
integral :: Integral i => i -> JSON
integral = unsafeToJSON . BI.decimal
{-# INLINABLE integral #-}

-- | json number value from RealFloat
--
-- >>> realFloat 235.12
-- "235.12"
realFloat :: RealFloat f => f -> JSON
realFloat = unsafeToJSON . BF.realFloat
{-# INLINABLE realFloat #-}

surround :: B.Builder -> B.Builder -> B.Builder -> B.Builder
surround pre suf bdy = pre `mappend` bdy `mappend` suf

dblQuote :: B.Builder -> B.Builder
dblQuote = surround q q
  where
    q = word16 quotationMark

-- | json text value from Text
--
-- >>> print $ text "foo\n"
-- "\"foo\\n\""
text :: T.Text -> JSON
text t = JSON $ \EncodeConfig{..} -> dblQuote $
    T.foldr (\c b -> escape isEscape c `mappend` b) mempty t
{-# INLINABLE text #-}

-- | json text value from LazyText
--
-- >>> print $ lazyText "bar\0"
-- "\"bar\\u0000\""
lazyText :: L.Text -> JSON
lazyText t = JSON $ \EncodeConfig{..} -> dblQuote $
    L.foldr (\c b -> escape isEscape c `mappend` b) mempty t
{-# INLINABLE lazyText #-}

-- | json text value from String
--
-- >>> print $ string ("baz\\" :: String)
-- "\"baz\\\\\""
string :: F.Foldable f => f Char -> JSON
string s = JSON $ \EncodeConfig{..} -> dblQuote $
    F.foldr (\c b -> escape isEscape c `mappend` b) mempty s
{-# INLINABLE string #-}

intersperse :: (F.Foldable f, Monoid m) => (a -> m) -> m -> f a -> m
intersperse f s a = F.foldr go (\n _ -> n) a mempty id
  where
    go i g = \_ j -> g (j $ f i) (\b -> j $ f i `mappend` s `mappend` b)

array' :: F.Foldable f => (a -> JSON) -> f a -> JSON
array' f a = JSON $ \cfg -> surround bra ket $
    intersperse (flip unJSON cfg . f) (word16 comma) a
  where
    bra = word16 $ ord '['
    ket = word16 $ ord ']'
{-# INLINABLE array' #-}

unsafeObject' :: F.Foldable f => (k -> T.Text) -> (a -> JSON) -> f (k, a) -> JSON
unsafeObject' kf vf o = JSON $ \cfg -> surround curly brace $
    intersperse (kv cfg) (word16 comma) o
  where
    curly = word16 $ ord '{'
    brace = word16 $ ord '}'
    colon = word16 $ ord ':'
    kv cfg (k, v) =
        unJSON (text $ kf k) cfg `mappend` colon `mappend` unJSON (vf v) cfg
{-# INLINABLE unsafeObject' #-}

object' :: F.Foldable f => (k -> T.Text) -> (a -> JSON) -> f (k, a) -> JSON
object' kf vf a = unsafeObject' id vf $
    F.foldr go (\_ out -> out) a Set.empty id []
  where
    go (k, v) g = \dict l ->
        if Set.member (kf k) dict
        then g dict l
        else g (Set.insert (kf k) dict) (l . ((kf k,v):))
{-# INLINABLE object' #-}

array :: F.Foldable f => f JSON -> JSON
array = array' id
{-# INLINABLE array #-}

-- | O(nlogn) convert to object
--
-- prior value is prevailed.
--
-- You could use 'unsafeObject' when could ensure unique key.
--
-- >>> object [("foo", integral 12), ("bar", bool True), ("foo", text "ignored")]
-- "{\"foo\":12,\"bar\":true}"
object :: F.Foldable f => f (T.Text, JSON) -> JSON
object = object' id id
{-# INLINABLE object #-}

-- | O(n) unique key list to object
--
-- >>> unsafeObject [("foo", integral 12), ("bar", bool True), ("foo", text "INVALID")]
-- "{\"foo\":12,\"bar\":true,\"foo\":\"INVALID\"}"
unsafeObject :: F.Foldable f => f (T.Text, JSON) -> JSON
unsafeObject = unsafeObject' id id
{-# INLINABLE unsafeObject #-}
