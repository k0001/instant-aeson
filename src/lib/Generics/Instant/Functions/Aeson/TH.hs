-- | NOTE: You need to add this to your module for this to work:
--
-- @
-- import qualified Data.Aeson
-- @
--
-- WARNING: Template Haskell is hard, and this module by no means try to be
-- truly comprehensive. If you can't get this TH to work as you want it to, try
-- manually using the tools exported from "Generics.Instant.Functions.Aeson".
module Generics.Instant.Functions.Aeson.TH
  ( deriveToJSON
  , deriveFromJSON
  ) where

import Language.Haskell.TH

--------------------------------------------------------------------------------

-- | Assuming a 'Generics.Instant.Representable' type like:
--
-- @
-- data Foo a where
--   Bar :: Foo Int
--   Qux :: Foo Char
--
-- Generics.Instant.TH.deriveAll ''Foo
-- @
--
-- You can derive the following instances generically:
--
-- @
-- 'deriveToJSON' [t|forall a. Foo a|]
--    ====> instance 'Data.Aeson.ToJSON' (Foo a)
--
-- 'deriveFromJSON' [t|Foo Int|]
--    ====> instance 'Data.Aeson.FromJSON' (Foo Int)
--
-- 'deriveFromJSON' [t|Foo Bar|]
--    ====> instance 'Data.Aeson.FromJSON' (Foo Bar)
-- @
--
-- Do not attempt to do something like @'deriveFromJSON' [t|forall a. Foo a|]@,
-- it won't work, as the 'FromJSON' instance needs to know, specifically, which
-- @Foo a@ it should be parsing into.
--
-- TODO: Write
-- @Generics.Instant.Functions.Aeson.deriveAll :: 'Name' -> 'Q' ['Dec']@
-- generating all of the above calls to 'deriveToJSON' and 'deriveFromJSON'.
deriveToJSON :: Q Type -> Q [Dec]
deriveToJSON qty0 = do
   (qcxt0, qty1) <- do
      ty0 <- qty0
      case ty0 of
         ForallT _ a ty1 -> pure (pure a, pure ty1)
         _ -> pure (cxt [], qty0)
   fmap pure $ instanceD
      qcxt0
      (appT (conT (mkName "Data.Aeson.ToJSON")) qty1)
      [ pragInlD (mkName "toJSON") Inlinable FunLike AllPhases
      , funD
          (mkName "toJSON")
          [clause
             []
             (normalB
                (varE (mkName "Generics.Instant.Functions.Aeson.gtoJSON")))
             []
          ]
      ]

deriveFromJSON :: Q Type -> Q [Dec]
deriveFromJSON qty0 = do
   do ty0 <- qty0
      case ty0 of
         ForallT {} ->
            error ("Generics.Instant.Functions.Aeson.TH.deriveFromJSON: \
                  \ unsupported type " ++ showsPrec 1 ty0 "")
         _ -> return ()
   fmap pure $ instanceD
      (cxt [])
      (appT (conT (mkName "Data.Aeson.FromJSON")) qty0)
      [ pragInlD (mkName "parseJSON") Inlinable FunLike AllPhases
      , funD
          (mkName "parseJSON")
          [clause
             []
             (normalB
                (varE (mkName "Generics.Instant.Functions.Aeson.gparseJSON")))
             []
          ]
      ]
