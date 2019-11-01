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
  , MultiParamTypeClasses
  , NoImplicitPrelude
  , RankNTypes
  , ScopedTypeVariables
  , TypeFamilies
  #-}

module XMonad.Util.Optics.Classy
    -- ( HasLayout, _layout
    -- , HasLayouts, _layouts
    -- , HasStack, _stack
    -- , HasTags, _tags
    -- , HasWindows, _windows
    -- , HasWorkspaceNames, _workspaceNames
    -- -- XConf Lenses:
    -- , _buttonActions
    -- , _xConfig
    -- , _currentEvent
    -- , _display
    -- , _focusedBorder, _normalBorder
    -- , _keyActions
    -- , _mouseFocused
    -- , _mousePosition
    -- , _theRoot
    -- -- XState
    -- , _dragging
    -- StackSet (WindowSet)
    -- Stack (Zipper)
    -- )
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
    (Layout, ManageHook, ScreenDetail, ScreenId, StateExtension, WindowSet, WorkspaceId, X, XConf (..), XConfig (..), XState (..))
import XMonad.StackSet
    (RationalRect (..), Screen (..), Stack (..), StackSet (..), Workspace (..))
import XMonad.Util.Optics.Types
    (Traversal, Lens, Simple, ScreenOf, WorkspaceOf, LayoutOf, ScreenDetailOf, ScreenIdOf, WindowOf, WorkspaceIdOf)

--- Functions:
import Data.Function (flip)


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
    (a ~ LayoutOf ta , b ~ LayoutOf tb) =>
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
    (a ~ LayoutOf ta, b ~ LayoutOf tb) =>
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
    (a ~ LayoutOf ta, b ~ LayoutOf tb) =>
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
  _workspaceNames :: Simple Traversal ta String

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
    type FloatingWindow ta
    _floating :: Simple Lens ta (Map (FloatingWindow ta) RationalRect)

class HasHidden ta where
    type HiddenItem ta
    _hidden :: Simple Lens ta [HiddenItem ta]

class
    (a ~ WorkspaceOf ta, b ~ WorkspaceOf tb) =>
    HasWorkspaces ta tb a b
    | ta -> a, tb -> b, ta b -> tb, tb a -> ta
  where
    _workspaces :: Traversal ta tb a b

----- XMonad.Core types -----

--- XConf instances:

instance HasXConfig XConf where
    _xConfig f s = (\ x -> s{ config = x }) <$> f (config s)

instance HasButtonActions XConf where
    _buttonActions f s =
        (\ x -> s{ buttonActions = x }) <$> f (buttonActions s)

instance HasCurrentEvent XConf where
    _currentEvent f s = (\ x -> s{ currentEvent = x }) <$> f (currentEvent s)

instance HasDisplay XConf where
    _display f s = (\ x -> s{ display = x }) <$> f (display s)

instance HasBorder XConf where
    _focusedBorder f s =
        (\ x -> s{ focusedBorder = x }) <$> f (focusedBorder s)
    _normalBorder f s =
        (\ x -> s{ normalBorder = x }) <$> f (normalBorder s)

instance HasKeyActions XConf where
    _keyActions f s = (\ x -> s{ keyActions = x }) <$> f (keyActions s)

instance HasMouseFocused XConf where
    _mouseFocused f s = (\ x -> s{ mouseFocused = x }) <$> f (mouseFocused s)

instance HasMousePosition XConf where
    _mousePosition f s =
        (\ x -> s{ mousePosition = x }) <$> f (mousePosition s)

instance HasTheRoot XConf where
    _theRoot f s = (\ x -> s{ theRoot = x }) <$> f (theRoot s)

instance HasWorkspaceNames XConf where
  _workspaceNames = _xConfig . _workspaceNames

--- XConfig Optics:

instance HasBorderColor (XConfig layout) where
    _focusedBorderColor f s =
        (\ x -> s{ focusedBorderColor = x }) <$> f (focusedBorderColor s)
    _normalBorderColor f s =
        (\ x -> s{ normalBorderColor = x }) <$> f (normalBorderColor s)

instance HasHandleEventHook (XConfig layout) where
    _handleEventHook f s =
        (\ x -> s{ handleEventHook = x }) <$> f (handleEventHook s)

instance HasKeys (XConfig layout) where
    _keys f s = (\ x -> s{ keys = x }) <$> f (keys s)

instance HasLayoutHook
    (XConfig layout) (XConfig layout')
    (layout Window) (layout' Window)
  where
    _layoutHook f s = (\ x -> s{ layoutHook = x }) <$> f (layoutHook s)

instance HasManageHook (XConfig layout) where
    _manageHook f s = (\ x -> s{ manageHook = x }) <$> f (manageHook s)

instance HasModMask (XConfig layout) where
    _modMask f s = (\ x -> s{ modMask = x }) <$> f (modMask s)

instance HasTerminal (XConfig layout) where
    _terminal f s = (\ x -> s{ terminal = x }) <$> f (terminal s)

instance HasWorkspaceNames (XConfig layout) where
    _workspaceNames f s = (\ x -> s{ workspaces = x }) <$> traverse f (workspaces s)


--- XState instances:

instance HasDragging XState where
    _dragging f s = (\ x -> s{ dragging = x }) <$> f (dragging s)

instance HasExtensibleState XState where
    _extensibleState f s =
        (\ x -> s{ extensibleState = x }) <$> f (extensibleState s)

instance HasMapped XState where
    _mapped f s = (\ x -> s{ mapped = x }) <$> f (mapped s)
    _waitingUnmap f s = (\ x -> s{ waitingUnmap = x }) <$> f (waitingUnmap s)

instance HasNumberLockMask XState where
    _numberlockMask f s =
        (\ x -> s{ numberlockMask = x }) <$> f (numberlockMask s)

instance HasWindowSet XState where
    _windowset f s = (\ x -> s{ windowset = x }) <$> f (windowset s)

instance HasVisible
    XState
    XState
    (Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail)
    (Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail)
  where
    _screens = _windowset . _screens
    _current = _windowset . _current
    _visible = _windowset . _visible

instance HasWorkspaces
    XState
    XState
    (Workspace WorkspaceId (Layout Window) Window)
    (Workspace WorkspaceId (Layout Window) Window)
  where
    _workspaces = _windowset . _workspaces

----- Optics for XMonad.StackSet -----

--- 'Stack' (list zipper) instances:

instance HasZipper (Stack a) where
    type ZipperItem (Stack a) = a
    _focus f s = (\ x -> s{ focus = x }) <$> f (focus s)
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

instance HasWindowSet (StackSet WorkspaceId (Layout Window) Window ScreenId ScreenDetail)
  where
    _windowset = id

instance HasFloating (StackSet tag layout window screenID screenDimensions) where
    type FloatingWindow (StackSet tag layout window screenID screenDimensions) =
        window
    _floating f s = (\ x -> s{ floating = x }) <$> f (floating s)

instance HasHidden (StackSet tag layout window screenID screenDimensions) where
    type HiddenItem (StackSet tag layout window screenID screenDimensions) =
        (Workspace tag layout window)
    _hidden f s = (\ x -> s{ hidden = x }) <$> f (hidden s)

instance HasVisible
    (StackSet tag layout window screenID screenDimensions)
    (StackSet tag layout window screenID' screenDimensions')
    (Screen tag layout window screenID screenDimensions)
    (Screen tag layout window screenID' screenDimensions')
  where
    _current f s = (\ x -> s{ current = x }) <$> f (current s)
    _visible f s = (\ x -> s{ visible = x }) <$> f (visible s)
    _screens f s =
        (\ (x :| xs) -> s { current = x, visible = xs })
        <$> f (current s :| visible s)

--- StackSet Traversals:

instance HasWorkspaces
    (StackSet tag layout window screenID screenDimensions)
    (StackSet tag' layout' window screenID screenDimensions)
    (Workspace tag layout window)
    (Workspace tag' layout' window)
  where
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


----- Other Trivial Instances -----

instance HasScreenDetail ScreenDetail ScreenDetail ScreenDetail ScreenDetail where
    _screenDetail = id

instance HasScreenId ScreenId ScreenId ScreenId ScreenId where
    _screenId = id

instance HasTag WorkspaceId WorkspaceId WorkspaceId WorkspaceId where
    _tag = id
