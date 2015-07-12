{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.Instant.Functions.Aeson
  ( -- $defaults
    gtoJSON
  , gparseJSON
    -- * Internals
  , GToJSON
  , GFromJSON
    -- ** Even more internal
  , GSumFromJSON
  , GSumToJSON
  , GSumSize
  ) where

import qualified Data.Aeson as Ae
import qualified Data.Aeson.Types as Ae
import           Data.Bits
import           Generics.Instant

--------------------------------------------------------------------------------
-- $defaults
--
-- You can use 'gtoJSON' and 'gparseJSON' as your generic 'Ae.toJSON' and
-- 'Ae.parseJSON' implementations for any 'Representable' type as follows:
--
-- @
-- instance 'Ae.ToJSON' MyType where toJSON = 'gtoJSON'
-- instance 'Ae.FromJSON' MyType where parseJSON = 'gparseJSON'
-- @

gtoJSON :: (Representable a, GToJSON (Rep a)) => a -> Ae.Value
gtoJSON = \a -> gtoJSON' (from a)
{-# INLINE gtoJSON #-}

gparseJSON :: (Representable a, GFromJSON (Rep a)) => Ae.Value -> Ae.Parser a
gparseJSON = \v -> fmap to (gparseJSON' v)
{-# INLINE gparseJSON #-}

--------------------------------------------------------------------------------

class GFromJSON a where
  gparseJSON' :: Ae.Value -> Ae.Parser a

instance GFromJSON Z where
  gparseJSON' _ = fail
    "Generics.Instant.Functions.Aeson.GFromJSON Z gparseJSON' - impossible"

instance GFromJSON U where
  gparseJSON' v = U <$ (Ae.parseJSON v :: Ae.Parser ())
  {-# INLINE gparseJSON' #-}

instance GFromJSON a => GFromJSON (CEq c p p a) where
  gparseJSON' v = gparseJSON' v >>= \a -> return (C a)
  {-# INLINE gparseJSON' #-}

instance {-# OVERLAPPABLE #-} GFromJSON (CEq c p q a) where
  gparseJSON' _ = fail
    "Generics.Instant.Functions.Aeson.GFtomJSON (CEq c p q a) gparseJSON' - impossible"

instance Ae.FromJSON a => GFromJSON (Var a) where
  gparseJSON' v = Ae.parseJSON v >>= \a -> return (Var a)
  {-# INLINE gparseJSON' #-}

instance Ae.FromJSON a => GFromJSON (Rec a) where
  gparseJSON' v = Ae.parseJSON v >>= \a -> return (Rec a)
  {-# INLINE gparseJSON' #-}

instance (GFromJSON a, GFromJSON b) => GFromJSON (a :*: b) where
  gparseJSON' v = Ae.parseJSON v >>= \(va, vb) ->
                  gparseJSON' va >>= \a ->
                  gparseJSON' vb >>= \b ->
                  return (a :*: b)
  {-# INLINE gparseJSON' #-}

-- Borrowed from the "binary" package, which borrowed this from "cereal".
instance
  ( GSumFromJSON a, GSumFromJSON b, GSumSize a, GSumSize b
  , GFromJSON a, GFromJSON b
  ) => GFromJSON (a :+: b)
  where
    gparseJSON' v = Ae.parseJSON v >>= \(code, v') ->
      let size = unTagged (sumSize :: Tagged (a :+: b) Integer)
      in if code < size
         then gsumParseJSON code size v'
         else fail "Generics.Instant.Functions.Aeson.GFromJSON (a :+: b) - \
                   \Unknown constructor"
    {-# INLINE gparseJSON' #-}

--------------------------------------------------------------------------------

class GToJSON a where
  gtoJSON' :: a -> Ae.Value

instance GToJSON Z where
  gtoJSON' _ = error
    "Generics.Instant.Functions.Aeson.GToJSON Z gtoJSON' - impossible"

instance GToJSON U where
  gtoJSON' U = Ae.toJSON ()
  {-# INLINE gtoJSON' #-}

instance GToJSON a => GToJSON (CEq c p p a) where
  gtoJSON' (C a) = gtoJSON' a
  {-# INLINE gtoJSON' #-}

instance {-# OVERLAPPABLE #-} GToJSON a => GToJSON (CEq c p q a) where
  gtoJSON' (C a) = gtoJSON' a
  {-# INLINE gtoJSON' #-}

instance Ae.ToJSON a => GToJSON (Var a) where
  gtoJSON' (Var a) = Ae.toJSON a
  {-# INLINE gtoJSON' #-}

instance Ae.ToJSON a => GToJSON (Rec a) where
  gtoJSON' (Rec a) = Ae.toJSON a
  {-# INLINE gtoJSON' #-}

instance (GToJSON a, GToJSON b) => GToJSON (a :*: b) where
  gtoJSON' (a :*: b) = Ae.toJSON (gtoJSON' a, gtoJSON' b)
  {-# INLINE gtoJSON' #-}

-- Borrowed from the "binary" package, which borrowed this from "cereal".
instance
  ( GSumToJSON a, GSumToJSON b, GSumSize a, GSumSize b
  , GToJSON a, GToJSON b
  ) => GToJSON (a :+: b)
  where
    gtoJSON' x =
      let size = unTagged (sumSize :: Tagged (a :+: b) Integer)
      in gsumToJSON 0 size x
    {-# INLINE gtoJSON' #-}

--------------------------------------------------------------------------------

class GSumFromJSON a where
  gsumParseJSON :: Integer -> Integer -> Ae.Value -> Ae.Parser a

instance
  ( GSumFromJSON a, GSumFromJSON b, GFromJSON a, GFromJSON b
  ) => GSumFromJSON (a :+: b)
  where
    {-# INLINE gsumParseJSON #-}
    gsumParseJSON !code !size v
      | code < sizeL = L <$> gsumParseJSON code           sizeL v
      | otherwise    = R <$> gsumParseJSON (code - sizeL) sizeR v
      where
        sizeL = size `shiftR` 1
        sizeR = size - sizeL

instance GFromJSON a => GSumFromJSON (CEq c p p a) where
  gsumParseJSON _ _ v = gparseJSON' v
  {-# INLINE gsumParseJSON #-}

instance {-# OVERLAPPABLE #-} GSumFromJSON (CEq c p q a) where
  gsumParseJSON _ _ _ = fail
    "Generics.Instant.Functions.Aeson.GSumFromJSON (CEq c p q a) - impossible"

--------------------------------------------------------------------------------

class GSumToJSON a where
  gsumToJSON :: Integer -> Integer -> a -> Ae.Value

instance
  ( GSumToJSON a, GSumToJSON b, GToJSON a, GToJSON b
  ) => GSumToJSON (a :+: b)
  where
    {-# INLINE gsumToJSON #-}
    gsumToJSON !code !size x =
      let sizeL = size `shiftR` 1
          sizeR = size - sizeL
      in case x of
           L l -> gsumToJSON code           sizeL l
           R r -> gsumToJSON (code + sizeL) sizeR r

instance GToJSON a => GSumToJSON (CEq c p p a) where
  gsumToJSON !code _ ca = Ae.toJSON (code, gtoJSON' ca)
  {-# INLINE gsumToJSON #-}

instance {-# OVERLAPPABLE #-} GToJSON a => GSumToJSON (CEq c p q a) where
  gsumToJSON !code _ ca = Ae.toJSON (code, gtoJSON' ca)
  {-# INLINE gsumToJSON #-}

--------------------------------------------------------------------------------

class GSumSize a where
  sumSize :: Tagged a Integer

newtype Tagged s b = Tagged { unTagged :: b }

instance (GSumSize a, GSumSize b) => GSumSize (a :+: b) where
  {-# INLINE sumSize #-}
  sumSize = Tagged (unTagged (sumSize :: Tagged a Integer) +
                    unTagged (sumSize :: Tagged b Integer))

instance GSumSize (CEq c p q a) where
  {-# INLINE sumSize #-}
  sumSize = Tagged 1
