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
    gtoJSONDefault
  , gparseJSONDefault
  , RepGToJSON
  , RepGFromJSON
    -- * Internals
  , GToJSON(gtoJSON)
  , GFromJSON(gparseJSON)
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
-- You can use 'gtoJSONDefault' and 'gparseJSONDefault' as your generic
-- 'Ae.toJSON' and 'Ae.parseJSON' implementations for any 'Representable'
-- type as follows:
--
-- @
-- instance 'Ae.ToJSON' MyType where toJSON = 'gtoJSONDefault'
-- instance 'Ae.FromJSON' MyType where parseJSON = 'gparseJSONDefault'
-- @

gtoJSONDefault :: (Representable a, GToJSON (Rep a)) => a -> Ae.Value
gtoJSONDefault = \a -> gtoJSON (from a)
{-# INLINABLE gtoJSONDefault #-}

gparseJSONDefault :: (Representable a, GFromJSON (Rep a)) => Ae.Value -> Ae.Parser a
gparseJSONDefault = \v -> fmap to (gparseJSON v)
{-# INLINABLE gparseJSONDefault #-}


-- | @'RepGFromJSON'@ is simply a synonym for
-- @('Representable' a, 'GFromJSON' ('Rep' a))@ with the convenient
-- kind @(* -> 'GHC.Exts.Constraint')@
class (Representable a, GFromJSON (Rep a)) => RepGFromJSON a
instance (Representable a, GFromJSON (Rep a)) => RepGFromJSON a

-- | @'RepGToJSON'@ is simply a synonym for
-- @('Representable' a, 'GToJSON' ('Rep' a))@ with the convenient
-- kind @(* -> 'GHC.Exts.Constraint')@
class (Representable a, GToJSON (Rep a)) => RepGToJSON a
instance (Representable a, GToJSON (Rep a)) => RepGToJSON a

--------------------------------------------------------------------------------

class GFromJSON a where
  gparseJSON :: Ae.Value -> Ae.Parser a

instance GFromJSON Z where
  gparseJSON _ = fail
    "Generics.Instant.Functions.Aeson.GFromJSON Z gparseJSON - impossible"

instance GFromJSON U where
  gparseJSON v = U <$ (Ae.parseJSON v :: Ae.Parser ())
  {-# INLINABLE gparseJSON #-}

instance GFromJSON a => GFromJSON (CEq c p p a) where
  gparseJSON v = gparseJSON v >>= \a -> return (C a)
  {-# INLINABLE gparseJSON #-}

instance {-# OVERLAPPABLE #-} GFromJSON (CEq c p q a) where
  gparseJSON _ = fail
    "Generics.Instant.Functions.Aeson.GFtomJSON (CEq c p q a) gparseJSON - impossible"

instance Ae.FromJSON a => GFromJSON (Var a) where
  gparseJSON v = Ae.parseJSON v >>= \a -> return (Var a)
  {-# INLINABLE gparseJSON #-}

instance Ae.FromJSON a => GFromJSON (Rec a) where
  gparseJSON v = Ae.parseJSON v >>= \a -> return (Rec a)
  {-# INLINABLE gparseJSON #-}

instance (GFromJSON a, GFromJSON b) => GFromJSON (a :*: b) where
  gparseJSON v = Ae.parseJSON v >>= \(va, vb) ->
                  gparseJSON va >>= \a ->
                  gparseJSON vb >>= \b ->
                  return (a :*: b)
  {-# INLINABLE gparseJSON #-}

-- Borrowed from the "binary" package, which borrowed this from "cereal".
instance
  ( GSumFromJSON a, GSumFromJSON b, GSumSize a, GSumSize b
  , GFromJSON a, GFromJSON b
  ) => GFromJSON (a :+: b)
  where
    gparseJSON v = Ae.parseJSON v >>= \(code, v') ->
      let size = unTagged (sumSize :: Tagged (a :+: b) Integer)
      in if code < size
         then gsumParseJSON code size v'
         else fail "Generics.Instant.Functions.Aeson.GFromJSON (a :+: b) - \
                   \Unknown constructor"
    {-# INLINABLE gparseJSON #-}

--------------------------------------------------------------------------------

class GToJSON a where
  gtoJSON :: a -> Ae.Value

instance GToJSON Z where
  gtoJSON _ = error
    "Generics.Instant.Functions.Aeson.GToJSON Z gtoJSON - impossible"

instance GToJSON U where
  gtoJSON U = Ae.toJSON ()
  {-# INLINABLE gtoJSON #-}

instance GToJSON a => GToJSON (CEq c p p a) where
  gtoJSON (C a) = gtoJSON a
  {-# INLINABLE gtoJSON #-}

instance {-# OVERLAPPABLE #-} GToJSON a => GToJSON (CEq c p q a) where
  gtoJSON (C a) = gtoJSON a
  {-# INLINABLE gtoJSON #-}

instance Ae.ToJSON a => GToJSON (Var a) where
  gtoJSON (Var a) = Ae.toJSON a
  {-# INLINABLE gtoJSON #-}

instance Ae.ToJSON a => GToJSON (Rec a) where
  gtoJSON (Rec a) = Ae.toJSON a
  {-# INLINABLE gtoJSON #-}

instance (GToJSON a, GToJSON b) => GToJSON (a :*: b) where
  gtoJSON (a :*: b) = Ae.toJSON (gtoJSON a, gtoJSON b)
  {-# INLINABLE gtoJSON #-}

-- Borrowed from the "binary" package, which borrowed this from "cereal".
instance
  ( GSumToJSON a, GSumToJSON b, GSumSize a, GSumSize b
  , GToJSON a, GToJSON b
  ) => GToJSON (a :+: b)
  where
    gtoJSON x =
      let size = unTagged (sumSize :: Tagged (a :+: b) Integer)
      in gsumToJSON 0 size x
    {-# INLINABLE gtoJSON #-}

--------------------------------------------------------------------------------

class GSumFromJSON a where
  gsumParseJSON :: Integer -> Integer -> Ae.Value -> Ae.Parser a

instance
  ( GSumFromJSON a, GSumFromJSON b, GFromJSON a, GFromJSON b
  ) => GSumFromJSON (a :+: b)
  where
    {-# INLINABLE gsumParseJSON #-}
    gsumParseJSON !code !size v
      | code < sizeL = L <$> gsumParseJSON code           sizeL v
      | otherwise    = R <$> gsumParseJSON (code - sizeL) sizeR v
      where
        sizeL = size `shiftR` 1
        sizeR = size - sizeL

instance GFromJSON a => GSumFromJSON (CEq c p p a) where
  gsumParseJSON _ _ v = gparseJSON v
  {-# INLINABLE gsumParseJSON #-}

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
    {-# INLINABLE gsumToJSON #-}
    gsumToJSON !code !size x =
      let sizeL = size `shiftR` 1
          sizeR = size - sizeL
      in case x of
           L l -> gsumToJSON code           sizeL l
           R r -> gsumToJSON (code + sizeL) sizeR r

instance GToJSON a => GSumToJSON (CEq c p p a) where
  gsumToJSON !code _ ca = Ae.toJSON (code, gtoJSON ca)
  {-# INLINABLE gsumToJSON #-}

instance {-# OVERLAPPABLE #-} GToJSON a => GSumToJSON (CEq c p q a) where
  gsumToJSON !code _ ca = Ae.toJSON (code, gtoJSON ca)
  {-# INLINABLE gsumToJSON #-}

--------------------------------------------------------------------------------

class GSumSize a where
  sumSize :: Tagged a Integer

newtype Tagged s b = Tagged { unTagged :: b }

instance (GSumSize a, GSumSize b) => GSumSize (a :+: b) where
  {-# INLINABLE sumSize #-}
  sumSize = Tagged (unTagged (sumSize :: Tagged a Integer) +
                    unTagged (sumSize :: Tagged b Integer))

instance GSumSize (CEq c p q a) where
  {-# INLINABLE sumSize #-}
  sumSize = Tagged 1
