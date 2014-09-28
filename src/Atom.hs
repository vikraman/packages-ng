module Portage.Atom where

import Prelude hiding (EQ, GT, LT)

-- | The category of a package, for example @dev-lang@
type Category = String

-- | The name of the package, for example @ghc@
type Package = String

-- | The compare pattern in a version, for example @>=@
data Compare = LT | LE | EQ | GE | GT deriving (Eq, Ord)

instance Show Compare where
  show LT = "<"
  show LE = "<="
  show EQ = "="
  show GE = ">="
  show GT = ">"

-- | The version string, which contains the compare pattern and the version
-- itself, for example @>=7.4.2@
data Version = Version Compare String deriving (Eq, Ord)

instance Show Version where
  show (Version cmp ver) = show cmp ++ ver

-- | The slot of a package, for example @0\/7.6.3@
type Slot = String

-- | The repository of a package, for example @gentoo@
type Repository = String

-- | The typeclass that captures an atom
class Atom' a where
  -- | The category is always present in an atom
  category :: a -> Category
  -- | The package name is always present in an atom
  package :: a -> Package
  -- | The version maybe present in an atom
  version :: a -> Maybe Version
  -- | The slot maybe present in an atom
  slot :: a -> Maybe Slot
  -- | The repository maybe present in an atom
  repository :: a -> Maybe Repository

-- | The real atom type
newtype Atom a = Atom a

instance Atom' a => Show (Atom a) where
  show (Atom a) = cmp ++ cat ++ "/" ++ pkg ++ ver ++ slt ++ repo
    where cat = category a
          pkg = package a
          cmp = case version a of
            Just (Version c _) -> show c
            Nothing -> ""
          ver = case version a of
            Just (Version _ v) -> '-' : v
            Nothing -> ""
          slt = case slot a of
            Just s -> "::" ++ s
            Nothing -> ""
          repo = case repository a of
            Just r -> "::" ++ r
            Nothing -> ""

instance Atom' a => Eq (Atom a) where
  (Atom x) == (Atom y) = (category x == category y) &&
                         (package x == package y) &&
                         (version x == version y) &&
                         (slot x == slot y) &&
                         (repository x == repository y)

instance Atom' a => Ord (Atom a) where
  (Atom x) <= (Atom y) = (category x <= category y) &&
                         (package x <= package y) &&
                         (version x <= version y) &&
                         (slot x <= slot y) &&
                         (repository x <= repository y)
