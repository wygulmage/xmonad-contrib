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

{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , LiberalTypeSynonyms
  , MultiParamTypeClasses
  , NoImplicitPrelude
  , RankNTypes
  , ScopedTypeVariables
  , TypeFamilies
  #-}

module XMonad.Util.Optics.Classy
    ( HasLayout, _layout
    , HasLayouts, _layouts
    , HasTags, _tags
    , HasWindows, _windows
    , HasWorkspaceNames, _workspaceNames
    -- -- XConf Lenses:
    , HasButtonActions, _buttonActions
    , HasXConfig, _xConfig
    , HasCurrentEvent, _currentEvent
    , HasDisplay, _display
    , HasBorder, _focusedBorder, _normalBorder
    , HasKeyActions, _keyActions
    , HasMouseFocused, _mouseFocused
    , HasMousePosition, _mousePosition
    , HasTheRoot, _theRoot
    -- -- XState
    , HasDragging, _dragging
    -- StackSet (WindowSet)
    -- Screen
    , HasScreenId, _screenId
    , HasScreenDetail, _screenDetail
    -- Workspace
    , HasStack, _stack
    , HasTag, _tag
    -- Stack (Zipper)
    )
  where

--- Classes:
import Control.Applicative (Applicative ((<*>), pure), (<**>))
import Control.Category ((.), id)
import Data.Functor (Functor, (<$>))
import Data.Traversable (traverse)
import XMonad.Core (LayoutClass)

--- Types:
import Prelude (Bool, Either, Int, Maybe, String)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Semigroup (All)
import Data.Set (Set)
import Graphics.X11.Xlib
    (Button, ButtonMask, Display, KeyMask, KeySym, Pixel, Position, Window)
import Graphics.X11.Xlib.Extras (Event)
import XMonad.Core
    (Layout, ManageHook, ScreenDetail, ScreenId, StateExtension, WindowSet, WindowSpace, WorkspaceId, X, XConf (..), XConfig (..), XState (..))
import XMonad.StackSet
    (RationalRect (..), Screen (..), Stack (..), StackSet (..), Workspace (..))
import XMonad.Util.Optics.Types
    (Traversal, Lens, Simple, ScreenOf, WorkspaceOf, ALayoutOf, ScreenDetailOf, ScreenIdOf, WindowOf, WorkspaceIdOf)

--- Functions:
import Data.Function (flip)
import qualified XMonad.Util.Optics as O


{-
To distinguish them from the existing record names, all optics in this module start with '_'.

What follows is a massive amount of boilerplate. I've indiscriminately made every record field into a class, regardless of how useful it seems. 'Cause I can't predict how it will be used.

A few classes comprise multiple optics: `HasBorder` has `_focusedBorder` and `_normalBorder`; `HasBorderColor` has `_focusedBorderColor` and `normalBorderColor`; `HasMapped` has `_mapped` and `_waitingUnmap`.

-}

------- Optic Classes -------

----- Lens Classes -----
-- Many of these are all too polymorphic, because the types they represent in XMonad are too polymorphic. Please respect the types they /should/ have.

class HasDisplay ta where
    _display :: Simple Lens ta Display

class HasTheRoot ta where
    _theRoot :: Simple Lens ta Window

class
    (a ~ ALayoutOf ta, b ~ ALayoutOf tb) =>
    HasLayout ta tb a b
    | ta -> a, tb -> b -- This is also given by the constraints above.
    , ta b -> tb, tb a -> ta
  where
    _layout :: Lens ta tb a b

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

-- HasStack might be better named 'MayHaveStack'.
class
    (a ~ WindowOf ta, b ~ WindowOf tb) =>
    HasStack ta tb a b | ta -> a, tb -> b, ta b -> tb, tb a -> ta where
    _stack :: Lens ta tb (Maybe (Stack a)) (Maybe (Stack b))
    -- Should be `Simple Lens ta (Maybe Stack Window)`

class
    (a ~ WorkspaceIdOf ta, b ~ WorkspaceIdOf tb) =>
    HasTag ta tb a b
    | ta -> a, tb -> b
    , ta b -> tb, tb a -> ta
  where
    _tag :: Lens ta tb a b
    -- Should be `Simple Lens ta WorkspaceId`

class HasButtonActions ta where
  _buttonActions :: Simple Lens ta (Map (KeyMask, Button) (Window -> X ()))

class HasXConfig ta where
    _xConfig :: Simple Lens ta (XConfig Layout)
    -- `_xConfig` instead of `_config` to (hopefully) avoid some confusion

class HasCurrentEvent ta where
    _currentEvent :: Simple Lens ta (Maybe Event)

class HasBorder ta where
    _focusedBorder, _normalBorder :: Simple Lens ta Pixel

class HasKeyActions ta where
    _keyActions :: Simple Lens ta (Map (KeyMask, KeySym) (X ()))

class HasMouseFocused ta where
    _mouseFocused :: Simple Lens ta Bool

class HasMousePosition ta where
    _mousePosition :: Simple Lens ta (Maybe (Position, Position))

class HasBorderColor ta where
    _focusedBorderColor, _normalBorderColor :: Simple Lens ta String

class HasKeys ta where
    _keys ::
        Simple Lens ta (XConfig Layout -> Map (ButtonMask, KeySym) (X ()))

class HasHandleEventHook ta where
    _handleEventHook :: Simple Lens ta (Event -> X All)

class
    (a ~ ALayoutOf ta, b ~ ALayoutOf tb) =>
    HasLayoutHook ta tb a b
    | ta -> a, tb -> b, ta b -> tb, tb a -> ta
  where
    _layoutHook :: Lens ta tb a b

class HasManageHook ta where
    _manageHook :: Simple Lens ta ManageHook

class HasModMask ta where
    _modMask :: Simple Lens ta KeyMask

class HasTerminal ta where
    _terminal :: Simple Lens ta String

class HasDragging ta where
    _dragging :: Simple Lens ta (Maybe (Position -> Position -> X (), X ()))

class HasExtensibleState ta where
    _extensibleState ::
        Simple Lens ta (Map String (Either String StateExtension))

class HasMapped ta where
    _mapped :: Simple Lens ta (Set Window)
    _waitingUnmap :: Simple Lens ta (Map Window Int)

class HasNumberLockMask ta where
    _numberlockMask :: Simple Lens ta KeyMask

class HasWindowSet ta where
    _windowset :: Simple Lens ta WindowSet

class HasZipper ta where
    type ZipperItem ta
    _focus :: Simple Lens ta (ZipperItem ta)
    _up, _down :: Simple Lens ta [ZipperItem ta]


----- Traversal Classes

class
    (a ~ ALayoutOf ta, b ~ ALayoutOf tb) =>
    HasLayouts ta tb a b
    | ta -> a, tb -> b
    , ta b -> tb, tb a -> ta
  where
    _layouts :: Traversal ta tb a b

class
    (a ~ WorkspaceIdOf ta, b ~ WorkspaceIdOf tb) =>
    HasTags ta tb a b
    | ta -> a, tb -> b
    , ta b -> tb, tb a -> ta
  where
    _tags :: Traversal ta tb a b
    -- should be `Simple Traversal ta WorkspaceId`

class
    (a ~ WindowOf ta, b ~ WindowOf tb) =>
    HasWindows ta tb a b
    | ta -> a, tb -> b
    , ta b -> tb, tb a -> ta
  where
    _windows :: Traversal ta tb a b
    -- should be `Simple Traversal ta Window`

class HasWorkspaceNames ta where
  -- `_workspaceNames` is used so that `_workspaces` can be the Traversal of Workspaces.
  _workspaceNames :: Simple Lens ta [WorkspaceId]

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

class HasHidden ta where
    _hidden :: Simple Lens ta [WorkspaceOf ta]

class
    (a ~ WorkspaceOf ta, b ~ WorkspaceOf tb) =>
    HasWorkspaces ta tb a b
    | ta -> a, tb -> b, ta b -> tb, tb a -> ta
  where
    _workspaces :: Traversal ta tb a b


----- XMonad.Core -----

--- XConf instances:

instance HasXConfig XConf where
    _xConfig = O._xConfig

instance HasButtonActions XConf where
    _buttonActions = O._buttonActions

instance HasCurrentEvent XConf where
    _currentEvent = O._currentEvent

instance HasDisplay XConf where
    _display = O._display

instance HasBorder XConf where
    _focusedBorder = O._focusedBorder
    _normalBorder = O._normalBorder

instance HasKeyActions XConf where
    _keyActions = O._keyActions

instance HasMouseFocused XConf where
    _mouseFocused = O._mouseFocused

instance HasMousePosition XConf where
    _mousePosition = O._mousePosition

instance HasTheRoot XConf where
    _theRoot = O._theRoot

instance HasWorkspaceNames XConf where
  _workspaceNames = _xConfig . _workspaceNames

--- XConfig Optics:

instance HasBorderColor (XConfig layout) where
    _focusedBorderColor = O._focusedBorderColor
    _normalBorderColor = O._normalBorderColor

instance HasHandleEventHook (XConfig layout) where
    _handleEventHook = O._handleEventHook

instance HasKeys (XConfig layout) where
    _keys = O._keys

instance HasLayoutHook
    (XConfig layout) (XConfig layout')
    (layout Window) (layout' Window)
  where
    _layoutHook = O._layoutHook

instance HasManageHook (XConfig layout) where
    _manageHook = O._manageHook

instance HasModMask (XConfig layout) where
    _modMask = O._modMask

instance HasTerminal (XConfig layout) where
    _terminal = O._terminal

instance HasWorkspaceNames (XConfig layout) where
    _workspaceNames = O._workspaceNames


--- XState instances:

instance HasDragging XState where
    _dragging = O._dragging

instance HasExtensibleState XState where
    _extensibleState = O._extensibleState

instance HasMapped XState where
    _mapped = O._mapped
    _waitingUnmap = O._waitingUnmap

instance HasNumberLockMask XState where
    _numberlockMask = O._numberlockMask

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

--- 'Stack' (list zipper) instances:

instance HasZipper (Stack a) where
    type ZipperItem (Stack a) = a
    _focus = O._focus
    _up = O._up
    _down = O._down

instance HasWindows (Stack a) (Stack b) a b where
    -- This is the missing `Traversable` instance for Stack.
    _windows = O.traverseStack


--- StackSet Lenses:

instance HasWindowSet WindowSet
  where
    _windowset = id

instance HasFloating (StackSet tag layout window screenID screenDimensions) where
    _floating = O._floating

instance HasHidden (StackSet tag layout window screenID screenDimensions) where
    _hidden f s = (\ x -> s{ hidden = x }) <$> f (hidden s)

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

_workspace :: Lens
    (Screen tag layout window screenID screenDimensions)
    (Screen tag' layout' window' screenID screenDimensions)
    (Workspace tag layout window)
    (Workspace tag' layout' window')
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

instance
    HasLayout
    (Workspace tag layout window) (Workspace tag layout' window)
    layout layout'
  where
    _layout = O._layout

instance HasStack
    (Workspace tag layout window) (Workspace tag layout window')
    window window'
  where
    _stack = O._stack

instance HasTag
    (Workspace workspaceID layout window)
    (Workspace workspaceID' layout window)
    workspaceID
    workspaceID'
  where
    _tag = O._tag

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


----- Other Trivial Instances -----

instance HasScreenDetail ScreenDetail ScreenDetail ScreenDetail ScreenDetail where
    _screenDetail = id

instance HasScreenId ScreenId ScreenId ScreenId ScreenId where
    _screenId = id

instance HasTag WorkspaceId WorkspaceId WorkspaceId WorkspaceId where
    _tag = id
