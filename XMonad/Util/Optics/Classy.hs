  -----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Util.Optics.Classy
-- Copyright    : © 2019 Keith Wygant
-- License      : Public Domain
--
-- Maintainer   : Keith Wygant
-- Stability    : unstable
-- Portability  : unportable
--
-- Provides Lenses and Traversals for common XMonad types.
-- It is designed to be used with an optics library like lens or microlens, but the lenses are useful on their own as traversals.
-- The 'Classy' optics are all methods. This has two benefits:
-- 1. Classy optics can be 'lifted' like MTL methods.
-- 2. Classy optics provide fine-grained constraints on a function's capabilities.
-----------------------------------------------------------------------------
{----------------------------------------------------------------------------

Examples
--------

Most existing operations in Stackset have simpler implementations in terms of the optics:
------------
`StackSet.integrate` and `StackSet.index`: toListOf _windows
`StackSet.with z f`: `views (_current . _stack) (maybe z f)`
`StackSet.modify z f`: `_current . _stack %~ (maybe z f)`
`StackSet.modify' f`: `_current . _stack . traverse %~ f`
`StackSet.peek`: `views (_current . _stack) (views _focus)`
`StackSet.screens`: `toListOf _screens`
`StackSet.workspaces`: `toListOf _workspaces`
`StackSet.mapWorkspace f`: `_workspaces %~ f`
`StackSet.allWindows`: `L.nub . toListOf _windows`
`StackSet.mapLayout f`: `_layouts %~ f`
`StackSet.currentTag`: `view (_current . _tag)`
`StackSet.renameTag t'`: `_tags %~ (\ t -> if t' == t then t' else t)`

----------------------------------------------------------------------------}

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

module XMonad.Util.Optics.Classy
    ( HasLayout, _layout
    , HasLayouts, _layouts
    , HasTags, _tags
    , HasWindows, _windows
    , HasWorkspaceNames, _workspaceNames
    , HasButtonActions, _buttonActions
    , HasXConfig, _xConfig
    , HasCurrentEvent, _currentEvent
    , HasDisplay, _display
    , HasBorder, _focusedBorder, _normalBorder
    , HasKeyActions, _keyActions
    , HasMouseFocused, _mouseFocused
    , HasMousePosition, _mousePosition
    , HasTheRoot, _theRoot
    , HasDragging, _dragging
    , HasVisible, _current, _visible, _screens
    , HasHidden, _hidden
    , HasFloating, _floating
    , HasWorkspaces, _workspaces
    , HasScreenId, _screenId
    , HasScreenDetail, _screenDetail
    , HasStack, _stack
    , HasTag, _tag
    , HasZipper, _focus, _up, _down
    )
  where

--- Classes:
import Control.Category (id, (.))
import Data.Functor ((<$>))
import Data.Traversable (traverse)
import XMonad.Util.Optics.Classy.Stack (HasFocus (..), HasZipper (..), HasWindows (..))
-- import XMonad.Util.Optics.Classy.Workspace
import XMonad.Util.Optics.HasXConfig
import XMonad.Util.Optics.HasXConf
import XMonad.Util.Optics.HasXState
import XMonad.Util.Optics.HasWindowSet

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


{-
To distinguish them from the existing record names, all optics in this module start with '_'.

What follows is a massive amount of boilerplate. I've indiscriminately made every record field into a class, regardless of how useful it seems. 'Cause I can't predict how it will be used.

A few classes comprise multiple optics: `HasBorder` has `_focusedBorder` and `_normalBorder`; `HasBorderColor` has `_focusedBorderColor` and `normalBorderColor`; `HasMapped` has `_mapped` and `_waitingUnmap`.

-}

------- Optic Classes -------

-- Many of these are too polymorphic, because the types they represent in XMonad are too polymorphic. Please respect the types they /should/ have.

class
    (a ~ ScreenIdOf ta , b ~ ScreenIdOf tb) =>
    HasScreenId ta tb a b
    | ta -> a, tb -> b
    , ta b -> tb, tb a -> ta
  where
    _screenId :: Lens ta tb a b
    -- Should be `Simple Lens ta ScreenId`

class
    (a ~ ScreenDetailOf ta , b ~ ScreenDetailOf tb) =>
    HasScreenDetail ta tb a b
    | ta -> a, tb -> b
    , ta b -> tb, tb a -> ta
  where
    _screenDetail :: Lens ta tb a b
    -- Should be `Simple Lens ta ScreenDetail`

class HasWindowSet ta where
    _windowset :: Simple Lens ta WindowSet

class
    (a ~ ScreenOf ta, b ~ ScreenOf tb) =>
    HasVisible ta tb a b
    | ta -> a, tb -> b, ta b -> tb, tb a -> ta
  where
    _current :: (ta ~ tb, a ~ b) => Lens ta tb a b
    -- ^ active visible item
    _visible :: (ta ~ tb, a ~ b) => Lens ta tb [a] [b]
    -- ^ all inactive visible items
    _screens :: Lens ta tb (NonEmpty a) (NonEmpty b)
    -- ^ A Lens to a non-empty list of Screens, starting with the focused screen
    -- to traverse the Screens, use `_screens . traverse`.
    -- It's redundant because you already have `_current` and `_visible`, so maybe it should be changed to a Traversal of the Screens. But for some purposes it may be more convenient to modify them together as a list. If not, I'll change it to a Traversal.


class HasFloating ta where
    _floating :: Simple Lens ta (Map (WindowOf ta) RationalRect)

class
    (HasWorkspaces ta ta (WorkspaceOf ta) (WorkspaceOf ta)) =>
    HasHidden ta
  where
    _hidden :: Simple Lens ta [WorkspaceOf ta]


----- XMonad.Core -----

--- XState instances:
-- XState is not parameterized, and instantiates layout as Layout (in WindowSet).

instance HasWindowSet XState where
    _windowset = O._windowset

instance HasVisible
    XState
    XState
    (Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail)
    (Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail)
  where
    _screens = _windowset . _screens
    _current = _windowset . _current
    _visible = _windowset . _visible

instance HasWorkspaces XState XState WindowSpace WindowSpace where
    _workspaces = _windowset . _workspaces

----- Optics for XMonad.StackSet -----

--- StackSet Lenses:

instance HasWindowSet WindowSet
  where
    _windowset = id

instance HasFloating (StackSet tag layout window screenID screenDimensions) where
    _floating = O._floating

instance HasHidden (StackSet tag layout window screenID screenDimensions) where
    _hidden = O._hidden

instance HasVisible
    (StackSet tag layout window screenID screenDimensions)
    (StackSet tag layout window screenID' screenDimensions')
    (Screen tag layout window screenID screenDimensions)
    (Screen tag layout window screenID' screenDimensions')
  where
    _current = O._current
    _visible = O._visible
    _screens = O._screens

--- StackSet Traversals:

instance HasWorkspaces
    (StackSet tag layout window screenID screenDimensions)
    (StackSet tag' layout' window screenID screenDimensions)
    (Workspace tag layout window)
    (Workspace tag' layout' window)
  where
    _workspaces = O._workspaces

instance HasLayouts
    (StackSet tag layout window screenID screenDimensions)
    (StackSet tag layout' window screenID screenDimensions)
    layout
    layout'
  where
    _layouts = _workspaces . _layouts


--- Screen Lenses:

instance HasScreenId
    (Screen tag layout window screenID screenDimensions)
    (Screen tag layout window screenID' screenDimensions)
    screenID
    screenID'
  where
    _screenId = O._screenId

instance HasScreenDetail
    (Screen tag layout window screenID screenDimensions)
    (Screen tag layout window screenID screenDimensions')
    screenDimensions
    screenDimensions'
  where
    _screenDetail = O._screenDetail

instance HasWorkspaces
    (Screen tag layout window screenID screenDimensions)
    (Screen tag' layout' window screenID screenDimensions)
    (Workspace tag layout window)
    (Workspace tag' layout' window)
instance HasWorkspace
    (Screen tag layout window screenID screenDimensions)
    (Screen tag' layout' window screenID screenDimensions)
    (Workspace tag layout window)
    (Workspace tag' layout' window)
  where
    _workspace = O._workspace

instance HasLayout
    (Screen tag layout window screenID screenDimensions)
    (Screen tag layout' window screenID screenDimensions)
    layout
    layout'
  where
    _layout = _workspace . _layout

instance HasLayouts
    (Screen tag layout window screenID screenDimensions)
    (Screen tag layout' window screenID screenDimensions)
    layout
    layout'
  where
    _layouts = _workspace . _layouts

instance HasWindows
    (Screen tag layout window screenID screenDimensions)
    (Screen tag layout window screenID screenDimensions)
    window
    window
  where
    _windows = _workspace . _windows

instance HasTags
    (Screen tag layout window screenID screenDimensions)
    (Screen tag' layout window screenID screenDimensions)
    tag
    tag'

instance HasTag
    (Screen tag layout window screenID screenDimensions)
    (Screen tag' layout window screenID screenDimensions)
    tag
    tag'
  where
    _tag = _workspace . _tag



----- Other Trivial Instances -----

instance HasScreenDetail ScreenDetail ScreenDetail ScreenDetail ScreenDetail where
    _screenDetail = id

instance HasScreenId ScreenId ScreenId ScreenId ScreenId where
    _screenId = id

instance HasTags WorkspaceId WorkspaceId WorkspaceId WorkspaceId
instance HasTag WorkspaceId WorkspaceId WorkspaceId WorkspaceId where
    _tag = id
