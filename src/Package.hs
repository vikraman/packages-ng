module Portage.Package where

import Portage.Atom

-- | @dev-lang\/ghc@
data CP = CP Category Package
instance Atom' CP where
  category (CP cat _) = cat
  package (CP _ pkg) = pkg
  version _ = Nothing
  slot _ = Nothing
  repository _ = Nothing

-- | @dev-lang\/ghc::gentoo@
data CPR = CPR Category Package Repository
instance Atom' CPR where
  category (CPR cat _ _) = cat
  package (CPR _ pkg _) = pkg
  version _ = Nothing
  slot _ = Nothing
  repository (CPR _ _ repo) = Just repo

-- | @dev-lang\/ghc:0\/7.6.3@
data CPS = CPS Category Package Slot
instance Atom' CPS where
  category (CPS cat _ _) = cat
  package (CPS _ pkg _) = pkg
  version _ = Nothing
  slot (CPS _ _ slt) = Just slt
  repository _ = Nothing

-- | @>=dev-lang\/ghc-7.6@
data CPV = CPV Category Package Version
instance Atom' CPV where
  category (CPV cat _ _) = cat
  package (CPV _ pkg _) = pkg
  version (CPV _ _ ver) = Just ver
  slot _ = Nothing
  repository _ = Nothing

-- | @dev-lang\/ghc:0\/7.6.3::gentoo@
data CPSR = CPSR Category Package Slot Repository
instance Atom' CPSR where
  category (CPSR cat _ _ _) = cat
  package (CPSR _ pkg _ _) = pkg
  version _ = Nothing
  slot (CPSR _ _ slt _) = Just slt
  repository (CPSR _ _ _ repo) = Just repo

-- | @>=dev-lang\/ghc-7.6.2::gentoo@
data CPVR = CPVR Category Package Version Repository
instance Atom' CPVR where
  category (CPVR cat _ _ _) = cat
  package (CPVR _ pkg _ _) = pkg
  version (CPVR _ _ ver _) = Just ver
  slot _ = Nothing
  repository (CPVR _ _ _ repo) = Just repo

-- | @>=dev-lang\/ghc-7.6.3:0\/7.6.3@
data CPVS = CPVS Category Package Version Slot
instance Atom' CPVS where
  category (CPVS cat _ _ _) = cat
  package (CPVS _ pkg _ _) = pkg
  version _ = Nothing
  slot (CPVS _ _ _ slt) = Just slt
  repository _ = Nothing

-- | @>=dev-lang\/ghc-7.6.3:0\/7.6.3::gentoo@
data CPVSR = CPVSR Category Package Version Slot Repository
instance Atom' CPVSR where
  category (CPVSR cat _ _ _ _) = cat
  package (CPVSR _ pkg _ _ _) = pkg
  version (CPVSR _ _ ver _ _) = Just ver
  slot (CPVSR _ _ _ slt _) = Just slt
  repository (CPVSR _ _ _ _ repo) = Just repo
