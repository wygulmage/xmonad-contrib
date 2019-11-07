{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LiberalTypeSynonyms    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module XMonad.Util.Optics.Classy.Stack
    ( HasWindows, _windows
    , HasZipper, _focus, _up, _down
    , HasFocus (..)
    )
  where

--- Classes:
import Control.Category (id, (.))
import Data.Functor ((<$>))
import Data.Traversable (traverse)

--- Types:
import Data.Map (Map)
import Data.Semigroup (All)
import Data.Set (Set)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Graphics.X11.Xlib
    (Button, ButtonMask, Display, KeyMask, KeySym, Pixel, Position, Window)
import Graphics.X11.Xlib.Extras (Event)
import Prelude (Bool, Either, Int, Maybe, String)
import XMonad.Core
    (Layout, ManageHook, ScreenDetail, ScreenId, StateExtension, WindowSet, WindowSpace, WorkspaceId, X, XConf, XConfig, XState)
import XMonad.StackSet (RationalRect, Screen, Stack, StackSet, Workspace)
import XMonad.Util.Optics.Types
    (ALayoutOf, Lens, ScreenDetailOf, ScreenIdOf, ScreenOf, Simple, Traversal, WindowOf, WorkspaceIdOf, WorkspaceOf)

--- Functions:
import qualified XMonad.Util.Optics as O


class HasFocus ta where
    -- An extraction Lens
    type FocusOf ta
    _focus :: Simple Lens ta (FocusOf ta)

class HasFocus ta => HasZipper ta where
    _up, _down :: Simple Lens ta [FocusOf ta]

class
    (a ~ WindowOf ta, b ~ WindowOf tb) =>
    HasWindows ta tb a b
    | ta -> a, tb -> b
    , ta b -> tb, tb a -> ta
  where
    _windows :: Traversal ta tb a b
    -- should be `Simple Traversal ta Window`
--- 'Stack' (list zipper) instances:

instance HasFocus (Stack a) where
    type FocusOf (Stack a) = a
    _focus = O._focus

instance HasZipper (Stack a) where
    _up = O._up
    _down = O._down

instance HasWindows (Stack a) (Stack b) a b where
    -- This is the missing `Traversable` instance for Stack.
    _windows = O.traverseStack

-- Aside for NonEmpty
instance HasFocus (NonEmpty a) where
    type FocusOf (NonEmpty a) = a
    _focus f ~(x :| xs) = (:| xs) <$> f x

instance HasWindows
    (Workspace tag layout window) (Workspace tag layout window)
    window window
  where
    _windows = _stack . traverse . _windows
