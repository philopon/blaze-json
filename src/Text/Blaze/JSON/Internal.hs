{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Blaze.JSON.Internal
    ( JSON(..), encode
    , unsafeToJSON
    , bool
    , null
    , integral
    , double, float
    , text
    , lazyText
    , array', array
    , object', object
    , unsafeObject', unsafeObject
    ) where

import Prelude hiding (null)

import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as BP

import qualified Data.Foldable as F
import Data.Typeable(Typeable)
import Data.Monoid

import Data.Word
import Data.Char(ord)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import qualified Data.Set as Set

-- $setup
-- >>> :set -XOverloadedStrings

-- | JSON encoding data type
newtype JSON = JSON { toBuilder :: B.Builder }
    deriving(Typeable)

encode :: JSON -> L.ByteString
encode = B.toLazyByteString . toBuilder
{-# INLINABLE encode #-}

instance Eq JSON where
    a == b = encode a == encode b
    {-# INLINABLE (==) #-}

instance Ord JSON where
    a `compare` b = encode a `compare` encode b
    {-# INLINABLE compare #-}

instance Show JSON where
    show = show . TL.decodeUtf8 . encode
    {-# INLINABLE show #-}

ascii2 :: (Char, Char) -> BP.BoundedPrim a
ascii2 cs = BP.liftFixedToBounded $ (const cs) BP.>$< BP.char7 BP.>*< BP.char7
{-# INLINE ascii2 #-}

ascii4 :: (Char, (Char, (Char, Char))) -> BP.BoundedPrim a
ascii4 cs = BP.liftFixedToBounded $ (const cs) BP.>$<
    BP.char7 BP.>*< BP.char7 BP.>*< BP.char7 BP.>*< BP.char7
{-# INLINE ascii4 #-}

ascii5 :: (Char, (Char, (Char, (Char, Char)))) -> BP.BoundedPrim a
ascii5 cs = BP.liftFixedToBounded $ (const cs) BP.>$<
    BP.char7 BP.>*< BP.char7 BP.>*< BP.char7 BP.>*< BP.char7 BP.>*< BP.char7
{-# INLINE ascii5 #-}

unsafeToJSON :: B.Builder -> JSON
unsafeToJSON = JSON
{-# INLINE unsafeToJSON #-}

-- | json null value
--
-- >>> null
-- "null"
null :: JSON
null = unsafeToJSON $ BP.primBounded (ascii4 ('n', ('u', ('l', 'l')))) ()
{-# INLINABLE null #-}

-- | json boolean value from Bool
--
-- >>> bool True
-- "true"
bool :: Bool -> JSON
bool = unsafeToJSON . BP.primBounded
    (BP.condB id
        (ascii4 ('t', ('r', ('u', 'e'))))
        (ascii5 ('f', ('a', ('l', ('s', 'e'))))))
{-# INLINABLE bool #-}

-- | json number value from Integral
--
-- >>> integral 32
-- "32"
integral :: Integral i => i -> JSON
integral = unsafeToJSON . B.integerDec . fromIntegral
{-# INLINABLE integral #-}

-- | json number value from float
--
-- >>> float 235.12
-- "235.12"
float :: Float -> JSON
float = unsafeToJSON . B.floatDec
{-# INLINABLE float #-}

-- | json number value from double
--
-- >>> double 235.12
-- "235.12"
double :: Double -> JSON
double = unsafeToJSON . B.doubleDec
{-# INLINABLE double #-}

encodeString :: (BP.BoundedPrim Word8 -> a -> B.Builder) -> a -> B.Builder
encodeString encodeUtf8BuilderEscaped t =
    B.char8 '"' <> encodeUtf8BuilderEscaped escapeAscii t <> B.char8 '"'
  where
    escapeAscii :: BP.BoundedPrim Word8
    escapeAscii =
        BP.condB (== c2w '\\' ) (ascii2 ('\\','\\')) $
        BP.condB (== c2w '\"' ) (ascii2 ('\\','"' )) $
        BP.condB (>= c2w '\x20') (BP.liftFixedToBounded BP.word8) $
        BP.condB (== c2w '\n' ) (ascii2 ('\\','n' )) $
        BP.condB (== c2w '\r' ) (ascii2 ('\\','r' )) $
        BP.condB (== c2w '\t' ) (ascii2 ('\\','t' )) $
        (BP.liftFixedToBounded hexEscape) -- fallback for chars < 0x20

    c2w = fromIntegral . ord

    hexEscape :: BP.FixedPrim Word8
    hexEscape = (\c -> ('\\', ('u', fromIntegral c))) BP.>$<
        BP.char8 BP.>*< BP.char8 BP.>*< BP.word16HexFixed
{-# INLINABLE encodeString #-}

-- | json text value from Text
--
-- >>> print $ text "foo\n"
-- "\"foo\\n\""
text :: T.Text -> JSON
text = JSON . encodeString T.encodeUtf8BuilderEscaped
{-# INLINABLE text #-}

-- | json text value from LazyText
--
-- >>> print $ lazyText "bar\0"
-- "\"bar\\u0000\""
lazyText :: TL.Text -> JSON
lazyText = JSON . encodeString TL.encodeUtf8BuilderEscaped
{-# INLINABLE lazyText #-}

intersperse :: (F.Foldable f, Monoid m) => (a -> m) -> m -> f a -> m
intersperse f s a = F.foldr go (\n _ -> n) a mempty id
  where
    go i g = \_ j -> g (j $ f i) (\b -> j $ f i <> s <> b)
{-# INLINABLE intersperse #-}

surround :: B.Builder -> B.Builder -> B.Builder -> B.Builder
surround pre suf bdy = pre <> bdy <> suf
{-# INLINABLE surround #-}

unsafeObject' :: F.Foldable f => (k -> T.Text) -> (a -> JSON) -> f (k, a) -> JSON
unsafeObject' kf vf = JSON . surround curly brace . intersperse keyValue (B.char8 ',')
  where
    curly = B.char8 '{'
    brace = B.char8 '}'
    colon = B.char8 ':'
    keyValue (k, v) = toBuilder (text $ kf k) <> colon <> toBuilder (vf v)
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

array' :: F.Foldable f => (a -> JSON) -> f a -> JSON
array' f = JSON . surround bra ket .
    intersperse (toBuilder . f) (B.char8 ',')
  where
    bra = B.char8 '['
    ket = B.char8 ']'
{-# INLINABLE array' #-}

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
