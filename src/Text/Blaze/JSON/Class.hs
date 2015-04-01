{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module Text.Blaze.JSON.Class where

import Prelude hiding(null)

import Text.Blaze.JSON.Internal

import qualified Data.Text as T
import qualified Data.Text.Lazy as L

import Data.Monoid
import Data.Int
import Data.Word
import qualified Data.Foldable as F

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Tree as Tree

class ToJSON a where
    toJSON :: a -> JSON

instance ToJSON a => ToJSON (Maybe a) where
    toJSON = maybe null toJSON
    {-# INLINE toJSON #-}

instance (ToJSON a, ToJSON b) => ToJSON (Either a b) where
    toJSON = object' id id . either left right
      where
        left  l = [("Left",  toJSON l)]
        right r = [("Right", toJSON r)]
    {-# INLINE toJSON #-}

instance ToJSON Bool where
    toJSON = bool
    {-# INLINE toJSON #-}

instance ToJSON () where
    toJSON _ = array' id []
    {-# INLINE toJSON #-}

instance ToJSON [Char] where
    toJSON = text . T.pack
    {-# INLINE toJSON #-}

instance ToJSON Char where
    toJSON = text . T.singleton
    {-# INLINE toJSON #-}

instance ToJSON Double where
    toJSON = double
    {-# INLINE toJSON #-}

instance ToJSON Float where
    toJSON = float
    {-# INLINE toJSON #-}

instance ToJSON Int where
    toJSON = integral
    {-# INLINE toJSON #-}

instance ToJSON Integer where
    toJSON = integral
    {-# INLINE toJSON #-}

instance ToJSON Int8 where
    toJSON = integral
    {-# INLINE toJSON #-}

instance ToJSON Int16 where
    toJSON = integral
    {-# INLINE toJSON #-}

instance ToJSON Int32 where
    toJSON = integral
    {-# INLINE toJSON #-}

instance ToJSON Int64 where
    toJSON = integral
    {-# INLINE toJSON #-}

instance ToJSON Word where
    toJSON = integral
    {-# INLINE toJSON #-}

instance ToJSON Word8 where
    toJSON = integral
    {-# INLINE toJSON #-}

instance ToJSON Word16 where
    toJSON = integral
    {-# INLINE toJSON #-}

instance ToJSON Word32 where
    toJSON = integral
    {-# INLINE toJSON #-}

instance ToJSON Word64 where
    toJSON = integral
    {-# INLINE toJSON #-}

instance ToJSON T.Text where
    toJSON = text
    {-# INLINE toJSON #-}

instance ToJSON L.Text where
    toJSON = lazyText
    {-# INLINE toJSON #-}

#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPABLE #-} (F.Foldable f, ToJSON a) => ToJSON (f a) where
#else
instance (F.Foldable f, ToJSON a) => ToJSON (f a) where
#endif
    toJSON = array' toJSON
    {-# INLINE toJSON #-}

instance ToJSON a => ToJSON (Set.Set a) where
    toJSON = toJSON . Set.toList
    {-# INLINE toJSON #-}

instance ToJSON IntSet.IntSet where
    toJSON = toJSON . IntSet.toList
    {-# INLINE toJSON #-}

instance ToJSON a => ToJSON (IntMap.IntMap a) where
    toJSON = toJSON . IntMap.toList
    {-# INLINE toJSON #-}

instance ToJSON v => ToJSON (Map.Map T.Text v) where
    toJSON = unsafeObject' id toJSON . Map.toList
    {-# INLINE toJSON #-}

instance ToJSON v => ToJSON (Map.Map L.Text v) where
    toJSON = unsafeObject' L.toStrict toJSON . Map.toList
    {-# INLINE toJSON #-}

instance ToJSON v => ToJSON (Map.Map String v) where
    toJSON = unsafeObject' T.pack toJSON . Map.toList
    {-# INLINE toJSON #-}

instance ToJSON a => ToJSON (Tree.Tree a) where
    toJSON (Tree.Node r b) = toJSON (r,b)
    {-# INLINE toJSON #-}

instance ToJSON JSON where
    toJSON = id
    {-# INLINE toJSON #-}

instance (ToJSON a, ToJSON b) => ToJSON (a, b) where
    toJSON (a,b) = array' id [toJSON a, toJSON b]
    {-# INLINE toJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (a, b, c) where
    toJSON (a,b,c) = array' id [toJSON a, toJSON b, toJSON c]
    {-# INLINE toJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON (a, b, c, d) where
    toJSON (a,b,c,d) = array' id [toJSON a, toJSON b, toJSON c, toJSON d]
    {-# INLINE toJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e) => ToJSON (a, b, c, d, e) where
    toJSON (a,b,c,d,e) = array' id [toJSON a, toJSON b, toJSON c, toJSON d, toJSON e]
    {-# INLINE toJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f) => ToJSON (a, b, c, d, e, f) where
    toJSON (a,b,c,d,e,f) = array' id [toJSON a, toJSON b, toJSON c, toJSON d, toJSON e, toJSON f]
    {-# INLINE toJSON #-}

instance ToJSON a => ToJSON (Dual a) where
    toJSON = toJSON . getDual
    {-# INLINE toJSON #-}

instance ToJSON a => ToJSON (First a) where
    toJSON = toJSON . getFirst
    {-# INLINE toJSON #-}

instance ToJSON a => ToJSON (Last a) where
    toJSON = toJSON . getLast
    {-# INLINE toJSON #-}
