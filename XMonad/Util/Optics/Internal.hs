module XMonad.Util.Optics.Internal where

import Control.Monad.Reader (MonadReader, asks)
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import XMonad.Util.Optics.Types

import Data.Coerce (coerce)


(%~) :: LensLike Identity ta tb a b -> (a -> b) -> ta -> tb
(%~) = coerce

to :: (a -> b) -> LensLike (Const r) a a b b
to f g = coerce (g . f)

view :: MonadReader ta m => LensLike (Const a) ta ta a a -> m a
view o = asks (coerce (o coerce))
