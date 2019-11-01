-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Util.Optics
-- Copyright    : © 2019 Keith Wygant
-- License      : Public Domain
--
-- Maintainer   : Keith Wygant
-- Stability    : unstable
-- Portability  : unportable
--
-- Provides Lenses and Traversals for common XMonad types.
-- It is designed to be used with an optics library like lens or microlens, but the lenses are useful on their own as traversals.
-----------------------------------------------------------------------------

{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , RankNTypes
  , ScopedTypeVariables
  #-}

module XMonad.Util.Optics
    ( HasLayout, _layout
    , HasLayouts, _layouts
    , HasStack, _stack
    , HasTags, _tags
    , HasWindows, _windows
    , HasWorkspaceNames, _workspaceNames
    -- XConf Lenses:
    , _buttonActions
    , _xConfig
    , _currentEvent
    , _display
    , _focusedBorder, _normalBorder
    , _keyActions
    , _mouseFocused
    , _mousePosition
    , _theRoot
    -- XState
    , _dragging
    -- StackSet (WindowSet)
    -- Stack (Zipper)
    ) where

import Control.Applicative ((<**>))

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Semigroup (All)
import Data.Set (Set)

import Graphics.X11.Xlib
    (Button, ButtonMask, Display, KeyMask, KeySym, Pixel, Position, Window)
import Graphics.X11.Xlib.Extras (Event)

import XMonad.Core
    (Layout, ManageHook, ScreenDetail, ScreenId, StateExtension, WindowSet, WorkspaceId, X, XConf (..), XConfig (..), XState (..))
import XMonad.StackSet (RationalRect (..), Screen (..), Stack (..), StackSet (..), Workspace (..))

----- Optics for XMonad.Core -----

--- XConf Optics:

_buttonActions :: Simple Lens XConf (Map (KeyMask, Button) (Window -> X ()))
_buttonActions f s = (\ x -> s{ buttonActions = x }) <$> f (buttonActions s)

_xConfig :: Simple Lens XConf (XConfig Layout)
-- `_xConfig` instead of `_config` to (hopefully) avoid some confusion
_xConfig f s = (\ x -> s{ config = x }) <$> f (config s)

_currentEvent :: Simple Lens XConf (Maybe Event)
_currentEvent f s = (\ x -> s{ currentEvent = x }) <$> f (currentEvent s)

instance HasDisplay XConf where
    _display f s = (\ x -> s{ display = x }) <$> f (display s)

_focusedBorder, _normalBorder :: Simple Lens XConf Pixel
_focusedBorder f s = (\ x -> s{ focusedBorder = x }) <$> f (focusedBorder s)
_normalBorder f s = (\ x -> s{ normalBorder = x }) <$> f (normalBorder s)

_keyActions :: Simple Lens XConf (Map (KeyMask, KeySym) (X ()))
_keyActions f s = (\ x -> s{ keyActions = x }) <$> f (keyActions s)

_mouseFocused :: Simple Lens XConf Bool
_mouseFocused f s = (\ x -> s{ mouseFocused = x }) <$> f (mouseFocused s)

_mousePosition :: Simple Lens XConf (Maybe (Position, Position))
_mousePosition f s = (\ x -> s{ mousePosition = x }) <$> f (mousePosition s)

instance HasTheRoot XConf where
    _theRoot f s = (\ x -> s{ theRoot = x }) <$> f (theRoot s)

instance HasWorkspaceNames XConf where
  _workspaceNames = _xConfig . _workspaceNames

--- XConfig Optics:

_focusedBorderColor, _normalBorderColor :: Simple Lens (XConfig layout) String
_focusedBorderColor f s =
    (\ x -> s{ focusedBorderColor = x }) <$> f (focusedBorderColor s)
_normalBorderColor f s =
    (\ x -> s{ normalBorderColor = x }) <$> f (normalBorderColor s)

_handleEventHook :: Simple Lens (XConfig layout) (Event -> X All)
_handleEventHook f s =
    (\ x -> s{ handleEventHook = x }) <$> f (handleEventHook s)

_keys :: Simple Lens (XConfig layout) (XConfig Layout -> Map (ButtonMask, KeySym) (X ()))
_keys f s = (\ x -> s{ keys = x }) <$> f (keys s)

_layoutHook ::
    Lens (XConfig layout) (XConfig layout') (layout Window) (layout' Window)
_layoutHook f s = (\ x -> s{ layoutHook = x }) <$> f (layoutHook s)

_manageHook :: Simple Lens (XConfig layout) ManageHook
_manageHook f s = (\ x -> s{ manageHook = x }) <$> f (manageHook s)

_modMask :: Simple Lens (XConfig layout) KeyMask
_modMask f s = (\ x -> s{ modMask = x }) <$> f (modMask s)

_terminal :: Simple Lens (XConfig layout) String
_terminal f s = (\ x -> s{ terminal = x }) <$> f (terminal s)

instance HasWorkspaceNames (XConfig layout) where
    _workspaceNames f s = (\ x -> s{ workspaces = x }) <$> traverse f (workspaces s)


--- XState Optics:

_dragging :: Simple Lens XState (Maybe (Position -> Position -> X (), X ()))
_dragging f s = (\ x -> s{ dragging = x }) <$> f (dragging s)

_extensibleState ::
    Simple Lens XState (Map String (Either String StateExtension))
_extensibleState f s =
    (\ x -> s{ extensibleState = x }) <$> f (extensibleState s)

_mapped :: Simple Lens XState (Set Window)
_mapped f s = (\ x -> s{ mapped = x }) <$> f (mapped s)

_numberlockMask :: Simple Lens XState KeyMask
_numberlockMask f s = (\ x -> s{ numberlockMask = x }) <$> f (numberlockMask s)

_waitingUnmap :: Simple Lens XState (Map Window Int)
_waitingUnmap f s = (\ x -> s{ waitingUnmap = x }) <$> f (waitingUnmap s)

_windowset :: Simple Lens XState WindowSet
_windowset f s = (\ x -> s{ windowset = x }) <$> f (windowset s)


----- Optics for XMonad.StackSet -----

--- 'Stack' (list zipper) Optics:

_focus :: Simple Lens (Stack a) a
_focus f s = (\ x -> s{ focus = x }) <$> f (focus s)

_up, _down :: Simple Lens (Stack a) [a]
_up f s = (\ x -> s{ up = x }) <$> f (up s)
_down f s = (\ x -> s{ down = x }) <$> f (down s)

instance HasWindows (Stack a) (Stack b) a b where
    -- This is the missing `Traversable` instance for Stack.
    _windows f s =
        (\ xu x xd -> s{ up = xu, focus = x, down = xd })
        <$> backwardsF (up s)
        <*> f (focus s)
        <*> traverse f (down s)
        where
          backwardsF (x : xs) = (:) <$> f x >*< backwardsF xs
          backwardsF _ = pure []
          (>*<) = flip (<**>)
          infixl 4 >*<


--- StackSet Lenses:

_current :: Simple Lens
    (StackSet tag (layout window) window screenID screenDimensions)
    (Screen tag (layout window) window screenID screenDimensions)
_current f s = (\ x -> s{ current = x }) <$> f (current s)

_floating :: Simple Lens
    (StackSet tag layout window screenID screenDimensions)
    (Map window RationalRect)
_floating f s = (\ x -> s{ floating = x }) <$> f (floating s)

_hidden :: Simple Lens
    (StackSet tag layout window screenID screenDimensions)
    [Workspace tag layout window]
_hidden f s = (\ x -> s{ hidden = x }) <$> f (hidden s)

_visible :: Simple Lens
    (StackSet tag layout window screenID screenDimensions)
    [Screen tag layout window screenID screenDimensions]
_visible f s = (\ x -> s{ visible = x }) <$> f (visible s)

_screens :: Lens
    (StackSet tag layout window screenID screenDimensions)
    (StackSet tag layout window screenID' screenDimensions')
    (NonEmpty (Screen tag layout window screenID screenDimensions))
    (NonEmpty (Screen tag layout window screenID' screenDimensions'))
-- ^ A Lens to a non-empty list of Screens, starting with the focused screen
-- to traverse the Screens, use `_screens . traverse`.
-- It's redundant because you already have `_current` and `_visible`, so maybe it should be changed to a Traversal of the Screens. But for some purposes it may be more convenient to modify them together as a list. If not, I'll change it to a Traversal.
_screens f s =
    (\ (x :| xs) -> s { current = x, visible = xs })
    <$> f (current s :| visible s)

--- StackSet Traversals:

_workspaces :: Traversal
    (StackSet tag layout window screenID screenDimensions)
    (StackSet tag' layout' window screenID screenDimensions)
    (Workspace tag layout window)
    (Workspace tag' layout' window)
_workspaces f s =
    (\ cur vis hid -> s{ current = cur, visible = vis, hidden = hid })
    <$> _workspace f (current s)
    <*> (traverse . _workspace) f (visible s)
    <*> traverse f (hidden s)

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
    _screenId f s = (\ x -> s{ screen = x }) <$> f (screen s)

instance HasScreenDetail
    (Screen tag layout window screenID screenDimensions)
    (Screen tag layout window screenID screenDimensions')
    screenDimensions
    screenDimensions'
  where
    _screenDetail f s = (\ x -> s{ screenDetail = x }) <$> f (screenDetail s)

_workspace :: Lens
    (Screen tag layout window screenID screenDimensions)
    (Screen tag' layout' window' screenID screenDimensions)
    (Workspace tag layout window)
    (Workspace tag' layout' window')
_workspace f s = (\ x -> s{ workspace = x }) <$> f (workspace s)

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

instance HasTag
    (Screen tag layout window screenID screenDimensions)
    (Screen tag' layout window screenID screenDimensions)
    tag
    tag'
  where
    _tag = _workspace . _tag

instance HasTags
    (Screen tag layout window screenID screenDimensions)
    (Screen tag' layout window screenID screenDimensions)
    tag
    tag'
  where
    _tags = _workspace . _tags


--- Workspace Lenses:

instance HasLayout
    (Workspace tag layout window) (Workspace tag layout' window)
    layout layout'
  where
    _layout f s = (\ x -> s{ layout = x }) <$> f (layout s)

instance HasStack
    (Workspace tag layout window) (Workspace tag layout window')
    window window'
  where
    _stack f s = (\ x -> s{ stack = x }) <$> f (stack s)

instance HasTag
    (Workspace workspaceID layout window)
    (Workspace workspaceID' layout window)
    workspaceID
    workspaceID'
  where
    _tag f s = (\ x -> s{ tag = x }) <$> f (tag s)

instance HasTags
    (Workspace workspaceID layout window)
    (Workspace workspaceID' layout window)
    workspaceID
    workspaceID'
  where
    _tags = _tag

instance HasLayouts
    (Workspace tag layout window) (Workspace tag layout' window)
    layout layout'
  where
    _layouts = _layout

instance HasWindows
    (Workspace tag layout window) (Workspace tag layout window)
    window window
  where
    _windows = _stack . traverse . _windows


------- Optic Classes -------

----- Lens Classes -----
-- Many of these are all too polymorphic, because the types they represent in XMonad are too polymorphic. Please respect the types they /should/ have.

class HasDisplay ta where
    _display :: Simple Lens ta Display

class HasTheRoot ta where
    _theRoot :: Simple Lens ta Window

class HasLayout ta tb a b
    | ta -> a, tb -> b
    , ta b -> tb, tb a -> ta
  where
    _layout :: Lens ta tb a b

class HasScreenId ta tb a b
    | ta -> a, tb -> b
    , ta b -> tb, tb a -> ta
  where
    _screenId :: Lens ta tb a b
    -- Should be `_screenId :: Simple Lens ta ScreenId`

class HasScreenDetail ta tb a b
    | ta -> a, tb -> b
    , ta b -> tb, tb a -> ta
  where
    _screenDetail :: Lens ta tb a b
    -- Should be `_screenDetail :: Simple Lens ta ScreenDetail`

-- HasStack might be better named 'MayHaveStack'.
class HasStack ta tb a b | ta -> a, tb -> b, ta b -> tb, tb a -> ta where
    _stack :: Lens ta tb (Maybe (Stack a)) (Maybe (Stack b))

class HasTag ta tb a b
    | ta -> a, tb -> b
    , ta b -> tb, tb a -> ta
  where
    _tag :: Lens ta tb a b
    -- Should be `_workspaceId :: Simple Lens ta WorkspaceId`


----- Traversal Classes

class HasLayouts ta tb a b
    | ta -> a, tb -> b
    , ta b -> tb, tb a -> ta
  where
    _layouts :: Traversal ta tb a b

class HasTags ta tb a b
    | ta -> a, tb -> b
    , ta b -> tb, tb a -> ta
  where
    _tags :: Traversal ta tb a b

class HasWindows ta tb a b
    | ta -> a, tb -> b
    , ta b -> tb, tb a -> ta
  where
    _windows :: Traversal ta tb a b
    -- should be `_windows :: Simple Traversal ta Window`

class HasWorkspaceNames ta where
  -- `_workspaceNames` is used so that `_workspaces` can be the Traversal of Workspaces.
  _workspaceNames :: Simple Traversal ta String


------- Non-exported Types (for documentation) --------

type LensLike m ta tb a b = (a -> m b) -> ta -> m tb
type Lens ta tb a b = forall m. Functor m => LensLike m ta tb a b
type Traversal ta tb a b = forall m. Applicative m => LensLike m ta tb a b
type Simple o ta a = o ta ta a a
