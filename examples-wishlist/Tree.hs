{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Tree where

import GHC.Generics (Generic)
import Prelude ((++), Eq, Show, (<$>), (<*>), pure)

import Test.QuickCheck (Arbitrary(arbitrary), oneof, Gen)

data Tree
    = Leaf
    | Branch Tree
             Tree
    deriving (Show, Eq, Generic)

instance Arbitrary Tree where
    arbitrary = oneof [pure Leaf, Branch <$> arbitrary <*> arbitrary]

subtrees :: Tree -> [Tree]
subtrees Leaf = []
subtrees (Branch t1 t2) = subtrees t1 ++ subtrees t2
