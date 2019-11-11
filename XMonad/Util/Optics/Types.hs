{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module XMonad.Util.Optics.Types where

import Control.Applicative (Applicative)
import Data.Functor (Functor)

------- Optical Types -------
type LensLike m ta tb a b = (a -> m b) -> ta -> m tb
type Lens       ta tb a b = forall m. Functor m => LensLike m ta tb a b
type Traversal  ta tb a b = forall m. Applicative m => LensLike m ta tb a b
type Simple   o ta    a   = o ta ta a a
