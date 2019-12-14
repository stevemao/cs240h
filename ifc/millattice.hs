{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Typeable
import Data.Set (Set)
import qualified Data.Set as Set
import LIO.Label
import Test.QuickCheck


data Level = Public | Secret | TopSecret
           deriving (Eq, Ord, Read, Show, Enum, Bounded)

data Compartment = Nuclear | Crypto
           deriving (Eq, Ord, Read, Show, Enum, Bounded)

data MilLabel = MilLabel { level :: Level
                         , compartments :: Set Compartment
                         } deriving (Eq, Ord, Read, Show, Typeable)

instance Label MilLabel where
  -- TODO: wrong
  lub a b 
    | level a < level b = a
    | otherwise = b
  glb a b 
    | level a > level b = a
    | otherwise = b
  canFlowTo a b = level a > level b


instance Arbitrary Level where
  arbitrary = arbitraryBoundedEnum
instance Arbitrary Compartment where
  arbitrary = arbitraryBoundedEnum
instance Arbitrary MilLabel where
  arbitrary = do l <- arbitrary
                 c <- Set.fromList `fmap` arbitrary
                 return $ MilLabel l c

lbot = MilLabel minBound Set.empty
ltop = MilLabel maxBound $ Set.fromList [ minBound .. maxBound ]
lsecnuke = MilLabel Secret $ Set.singleton Nuclear
lseccrypto = MilLabel Secret $ Set.singleton Crypto

