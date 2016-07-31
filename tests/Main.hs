{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import qualified Data.Aeson
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Types as Ae
import Data.Proxy

import qualified Generics.Instant as GI
import qualified Generics.Instant.TH as GI
import Generics.Instant.Functions.Aeson (GFromJSON, gparseJSONDefault,
                                         GToJSON, gtoJSONDefault,
                                         RepGToJSON, RepGFromJSON)

--------------------------------------------------------------------------------
-- orphans

data Unit_Unit_
instance GI.Constructor Unit_Unit_ where conName _ = "()"
instance GI.Representable () where
  type Rep () = GI.U
  from () = GI.U
  to GI.U = ()

GI.deriveAll ''Either

instance Arbitrary (Proxy a) where arbitrary = return Proxy

#if !MIN_VERSION_aeson(0,11,1)
instance Ae.ToJSON (Proxy a) where toJSON _ = Ae.toJSON ()
instance Ae.FromJSON (Proxy a) where parseJSON v = Ae.parseJSON v >>= \() -> return Proxy
#endif

--------------------------------------------------------------------------------
-- many constructors, recursive

data ZZ = ZZ1 Int | ZZ2 Char | ZZ3 ZZ deriving (Show, Eq)
GI.deriveAll ''ZZ
instance Arbitrary ZZ where
  arbitrary = QC.oneof [ ZZ1 <$> QC.arbitrary
                       , ZZ2 <$> QC.arbitrary
                       , ZZ3 <$> QC.arbitrary ]

instance Ae.ToJSON ZZ where toJSON = gtoJSONDefault
instance Ae.FromJSON ZZ where parseJSON = gparseJSONDefault

--------------------------------------------------------------------------------
-- GADT

data Foo1 (a :: *) :: * where
  Foo1_1 :: Bool -> Foo1 Bool
  Foo1_2 :: a -> Foo1 a
GI.deriveAll ''Foo1
deriving instance Eq a => Eq (Foo1 a)
deriving instance Show a => Show (Foo1 a)
instance QC.Arbitrary (Foo1 Bool) where
  arbitrary = Foo1_1 <$> QC.arbitrary
instance {-# OVERLAPPABLE #-} (QC.Arbitrary a, GI.Representable a) => QC.Arbitrary (Foo1 a) where
  arbitrary = Foo1_2 <$> QC.arbitrary


data Foo2 (a :: *) (b :: *) :: * where
  Foo2_1 :: a -> Char -> Foo2 a Int
  Foo2_2 :: a -> b -> Foo2 a b
GI.deriveAll ''Foo2
deriving instance (Eq a, Eq b) => Eq (Foo2 a b)
deriving instance (Show a, Show b) => Show (Foo2 a b)
instance (QC.Arbitrary a, GI.Representable a) => QC.Arbitrary (Foo2 a Int) where
  arbitrary = Foo2_1 <$> QC.arbitrary <*> QC.arbitrary
instance {-# OVERLAPPABLE #-} (QC.Arbitrary a, GI.Representable a, QC.Arbitrary b, GI.Representable b) => QC.Arbitrary (Foo2 a b) where
  arbitrary = Foo2_2 <$> QC.arbitrary <*> QC.arbitrary


data Bar1 (a :: Bool) :: * where
  Bar1_1 :: Char -> Bar1 'True
  Bar1_2 :: Int -> Bar1 'False
GI.deriveAll ''Bar1
deriving instance Eq (Bar1 a)
deriving instance Show (Bar1 a)
instance QC.Arbitrary (Bar1 'True) where
  arbitrary = Bar1_1 <$> QC.arbitrary
instance QC.Arbitrary (Bar1 'False) where
  arbitrary = Bar1_2 <$> QC.arbitrary

data Bar2 (a :: k1) (b :: Bool) :: * where
  Bar2_1 :: Int -> Proxy a -> Bar2 a 'True
  Bar2_2 :: String -> Proxy a -> Bar2 a 'False
GI.deriveAll ''Bar2
deriving instance Eq (Bar2 a b)
deriving instance Show (Bar2 a b)
instance (QC.Arbitrary a) => QC.Arbitrary (Bar2 a 'True) where
  arbitrary = Bar2_1 <$> QC.arbitrary <*> QC.arbitrary
instance (QC.Arbitrary a) => QC.Arbitrary (Bar2 a 'False) where
  arbitrary = Bar2_2 <$> QC.arbitrary <*> QC.arbitrary


--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "QuickCheck - prop_IdJSON"
  [ -- QC.testProperty "()" (prop_IdJSON :: () -> Bool)
--   , QC.testProperty "Bool" (prop_IdJSON :: Bool -> Bool)
--   , QC.testProperty "Char" (prop_IdJSON :: Char -> Bool)
--   , QC.testProperty "Float" (prop_IdJSON :: Float -> Bool)
--   , QC.testProperty "Int" (prop_IdJSON :: Int -> Bool)
    QC.testProperty "Maybe ()" (prop_IdJSON :: Maybe () -> Bool)
  , QC.testProperty "Maybe Bool" (prop_IdJSON :: Maybe Bool -> Bool)
  , QC.testProperty "Maybe Char" (prop_IdJSON :: Maybe Char -> Bool)
  , QC.testProperty "Maybe Float" (prop_IdJSON :: Maybe Float -> Bool)
  , QC.testProperty "Maybe Int" (prop_IdJSON :: Maybe Int -> Bool)
  , QC.testProperty "Maybe ZZ" (prop_IdJSON :: Maybe ZZ -> Bool)
  , QC.testProperty "[()]" (prop_IdJSON :: [()] -> Bool)
  , QC.testProperty "[Bool]" (prop_IdJSON :: [Bool] -> Bool)
  , QC.testProperty "[Char]" (prop_IdJSON :: [Char] -> Bool)
  , QC.testProperty "[Float]" (prop_IdJSON :: [Float] -> Bool)
  , QC.testProperty "[Int]" (prop_IdJSON :: [Int] -> Bool)
  , QC.testProperty "[ZZ]" (prop_IdJSON :: [ZZ] -> Bool)
  , QC.testProperty "Foo1 Int" (prop_IdJSON :: Foo1 Int -> Bool)
  , QC.testProperty "Foo1 Char" (prop_IdJSON :: Foo1 Char -> Bool)
  , QC.testProperty "Foo2 Float Int" (prop_IdJSON :: Foo2 Float Int -> Bool)
  , QC.testProperty "Foo2 Bool Char" (prop_IdJSON :: Foo2 Bool Char -> Bool)
  , QC.testProperty "Bar1 'True" (prop_IdJSON :: Bar1 'True -> Bool)
  , QC.testProperty "Bar1 'False" (prop_IdJSON :: Bar1 'False -> Bool)
  , QC.testProperty "Bar2 Int 'True" (prop_IdJSON :: Bar2 Int 'True -> Bool)
  , QC.testProperty "Bar2 Float 'False" (prop_IdJSON :: Bar2 Float 'False -> Bool)
  ]

prop_IdJSON :: (Eq a, Show a, RepGToJSON a, RepGFromJSON a) => a -> Bool
prop_IdJSON a = maybe False (== a) $ Ae.parseMaybe gparseJSONDefault (gtoJSONDefault a)
