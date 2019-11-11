{-# LANGUAGE
    NoImplicitPrelude
  , TypeFamilies
  #-}

module XMonad.Util.Optics.HasFocus
  where

import Control.Applicative ((<*>), liftA2)
import Data.Functor ((<$>))

import qualified XMonad.Util.Optics.Concrete as O

import Data.List.NonEmpty (NonEmpty ((:|)))

import XMonad.StackSet (Screen, Stack (..), StackSet (..), traverseStack)
import XMonad.Util.Optics.Types


class HasFocus a where
    type FocusOf a
    _focus :: Simple Lens a (FocusOf a)
    -- _unfocused :: Simple Traversal a (FocusOf a)
    -- _all :: Simple Traversal a (FocusOf a)


instance HasFocus (NonEmpty a) where
    type FocusOf (NonEmpty a) = a
    _focus f ~(x :| xs) = (:| xs) <$> f x
    -- _unfocused f ~(x :| xs) = (x :|) <$> traverse f xs
    -- _all = traverse

instance HasFocus (Stack a) where
    type FocusOf (Stack a) = a
    _focus = O._focus
    -- _unfocused f ~s@(Stack x _ _) =
        -- liftA2 (Stack x) (O._up f s) (O._down f s)
    -- _all = traverseStack

instance HasFocus (StackSet (..)) where
    type FocusOf (StackSet workspaceId layout window screenId screenDimensions) =
      (Screen workspaceId layout window screenId screenDimensions)
    _focus = O._current
