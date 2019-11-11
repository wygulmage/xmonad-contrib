{- |
Copyright    : © 2019 Keith Wygant
License      : Public Domain

Maintainer   : Keith Wygant
Stability    : unstable
Portability  : unportable

Provides Lenses and Traversals for common XMonad types. 'Concrete' as opposed to 'classy': these optics are all functions, not methods.
Designed to be used with an optics library like lens or microlens, but the lenses are useful on their own as traversals.
-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module XMonad.Util.Optics.Concrete

{----------------------------------------------------------------------------

Examples with functions from lens or microlens
----------------------------------------------

Map a function @f@ over the current stack if it isn't empty (equivalent to 'modify''):
@
'_index' %~ f
@

Map a function @f@ over the 'Workspace's in a 'StackSet' (equivalent to 'mapWorkspace'):
@
'_workspaces' %~ f
@

Get a list of all 'Workspace's in a 'StackSet' (equivalent to 'workspaces'):
@
toListOf '_workspaces'
@

Map a function @f@ over the layouts in a 'StackSet' (equivalent to 'mapLayout'):
@
'_workspaces' . '_layout' %~ f
@

Equivalent to @'StackSet.with' z f@ and @'ask' ('with' z f)@:
@
views ('_current' . '_workspace' . '_stack') (maybe z f)
@

----------------------------------------------------------------------------}

    -- XConf Lenses:
    ( _buttonActions
    , _xConfig
    , _currentEvent
    , _display
    , _focusedBorder, _normalBorder
    , _keyActions
    , _mouseFocused
    , _mousePosition
    , _theRoot
    -- XConfig Lenses:
    , _borderWidth
    , _clickJustFocuses
    , _clientMask
    , _focusedBorderColor, _normalBorderColor
    , _focusFollowsMouse
    , _handleEventHook
    , _handleExtraArgs
    , _keys
    , _layoutHook
    , _logHook
    , _manageHook
    , _modMask
    , _mouseBindings
    , _rootMask
    , _startupHook
    , _terminal
    , _workspaceNames
    -- XState
    , _dragging
    , _extensibleState
    , _mapped
    , _waitingUnmap
    , _numberlockMask
    , _windowset
    -- StackSet (WindowSet)
    , _current
    , _floating
    , _screens
    , _visible
    , _hidden
    , _workspaces
    , _index
    -- Screen
    , _screenId
    , _screenDetail
    , _workspace
    -- Workspace
    , _layout
    , _stack
    , _tag
    -- Stack (Zipper)
    , _focus
    , _up
    , _down
    , traverseStack
    )
  where


import Control.Applicative ( (<**>) )

import Data.List.NonEmpty ( NonEmpty ((:|)) )
import Data.Map ( Map )
import Data.Semigroup ( All )
import Data.Set ( Set )

import Graphics.X11.Xlib
    ( Button
    , ButtonMask
    , Dimension
    , Display
    , EventMask
    , KeyMask
    , KeySym
    , Pixel
    , Position
    , Window
    )
import Graphics.X11.Xlib.Extras ( Event )

import XMonad.Core
    ( Layout
    , ManageHook
    , ScreenDetail
    , ScreenId
    , StateExtension
    , WindowSet
    , WorkspaceId
    , X
    , XConf (..)
    , XConfig (..)
    , XState (..)
    )
import XMonad.StackSet
    ( RationalRect (..)
    , Screen (..)
    , Stack (..)
    , StackSet (..)
    , Workspace (..)
    )

import XMonad.Util.Optics.Types
import XMonad.Util.Optics.Internal


----- Optics for XMonad.Core -----

--- XConf Optics:

_buttonActions :: Simple Lens XConf (Map (KeyMask, Button) (Window -> X ()))
_buttonActions f s = (\ x -> s{ buttonActions = x }) <$> f (buttonActions s)

_xConfig :: Simple Lens XConf (XConfig Layout)
-- `_xConfig` instead of `_config` to (hopefully) avoid some confusion
_xConfig f s = (\ x -> s{ config = x }) <$> f (config s)

_currentEvent :: Simple Lens XConf (Maybe Event)
_currentEvent f s = (\ x -> s{ currentEvent = x }) <$> f (currentEvent s)

_display :: Simple Lens XConf Display
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

_theRoot :: Simple Lens XConf Window
_theRoot f s = (\ x -> s{ theRoot = x }) <$> f (theRoot s)

--- XConfig Optics:

_borderWidth :: Simple Lens (XConfig layout) Dimension
_borderWidth f s = (\ x -> s{ borderWidth = x }) <$> f (borderWidth s)

_clickJustFocuses :: Simple Lens (XConfig layout) Bool
_clickJustFocuses f s =
    (\ x -> s{ clickJustFocuses = x }) <$> f (clickJustFocuses s)

_clientMask :: Simple Lens (XConfig layout) EventMask
_clientMask f s = (\ x -> s{ clientMask = x }) <$> f (clientMask s)

_focusedBorderColor, _normalBorderColor :: Simple Lens (XConfig layout) String
_focusedBorderColor f s =
    (\ x -> s{ focusedBorderColor = x }) <$> f (focusedBorderColor s)
_normalBorderColor f s =
    (\ x -> s{ normalBorderColor = x }) <$> f (normalBorderColor s)

_focusFollowsMouse :: Simple Lens (XConfig layout) Bool
_focusFollowsMouse f s =
    (\ x -> s{ focusFollowsMouse = x }) <$> f (focusFollowsMouse s)

_handleEventHook :: Simple Lens (XConfig layout) (Event -> X All)
_handleEventHook f s =
    (\ x -> s{ handleEventHook = x }) <$> f (handleEventHook s)

_handleExtraArgs :: Simple Lens
    (XConfig layout)
    ([String] -> XConfig Layout -> IO (XConfig Layout))
_handleExtraArgs f s =
    (\ x -> s{ handleExtraArgs = x }) <$> f (handleExtraArgs s)

_keys :: Simple Lens (XConfig layout) (XConfig Layout -> Map (ButtonMask, KeySym) (X ()))
_keys f s = (\ x -> s{ keys = x }) <$> f (keys s)

_layoutHook ::
    Lens (XConfig layout) (XConfig layout') (layout Window) (layout' Window)
_layoutHook f s = (\ x -> s{ layoutHook = x }) <$> f (layoutHook s)

_logHook :: Simple Lens (XConfig layout) (X ())
_logHook f s = (\ x -> s{ logHook = x }) <$> f (logHook s)

_manageHook :: Simple Lens (XConfig layout) ManageHook
_manageHook f s = (\ x -> s{ manageHook = x }) <$> f (manageHook s)

_modMask :: Simple Lens (XConfig layout) KeyMask
_modMask f s = (\ x -> s{ modMask = x }) <$> f (modMask s)

_mouseBindings :: Simple Lens
    (XConfig layout)
    (XConfig Layout -> Map (ButtonMask, Button) (Window -> X ()))
_mouseBindings f s = (\ x -> s{ mouseBindings = x }) <$> f (mouseBindings s)

_rootMask :: Simple Lens (XConfig layout) EventMask
_rootMask f s = (\ x -> s{ rootMask = x }) <$> f (rootMask s)

_startupHook :: Simple Lens (XConfig layout) (X ())
_startupHook f s = (\ x -> s{ startupHook = x }) <$> f (startupHook s)

_terminal :: Simple Lens (XConfig layout) String
_terminal f s = (\ x -> s{ terminal = x }) <$> f (terminal s)

_workspaceNames :: Simple Lens (XConfig layout) [String]
_workspaceNames f s = (\ x -> s{ workspaces = x }) <$> f (workspaces s)


--- XState Lenses:

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

traverseStack :: Traversal (Stack a) (Stack b) a b
traverseStack f s =
        (\ xu x xd -> s{ up = xu, focus = x, down = xd })
        <$> backwardsF (up s)
        <*> f (focus s)
        <*> traverse f (down s)
        where
          -- XMonad does not expose its dependence on 'transformers'.
          backwardsF (x : xs) = (:) <$> f x >*< backwardsF xs
          backwardsF _        = pure []
          (>*<) = flip (<**>)
          infixl 4 >*<


--- StackSet Lenses:

_current :: Simple Lens
    (StackSet tag layout window screenID screenDimensions)
    (Screen tag layout window screenID screenDimensions)
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
-- This is perhaps what '_visible' /should/ be: A Lens to all visible screens, with the active one in a distinguished position.
_screens f s =
    (\ (x :| xs) -> s{ current = x, visible = xs })
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

_tags :: Traversal
    (StackSet tag layout window screenID screenDimensions)
    (StackSet tag' layout window screenID screenDimensions)
    tag
    tag'
_tags = _workspaces . _tag

_layouts :: Traversal
    (StackSet tag layout window screenID screenDimensions)
    (StackSet tag layout' window screenID screenDimensions)
    layout
    layout'
_layouts = _workspaces . _layout

_index :: Simple Traversal
    (StackSet tag layout window screenID screenDimensions)
    window
-- per @index :: StackSet tag layout window screenID screenDimensions -> [window]@
_index = _current . _workspace . _stack . traverse . traverseStack


--- Screen Lenses:

_screenId :: Lens
    (Screen tag layout window screenID screenDimensions)
    (Screen tag layout window screenID' screenDimensions)
    screenID
    screenID'
_screenId f s = (\ x -> s{ screen = x }) <$> f (screen s)

_screenDetail :: Lens
    (Screen tag layout window screenID screenDimensions)
    (Screen tag layout window screenID screenDimensions')
    screenDimensions
    screenDimensions'
_screenDetail f s = (\ x -> s{ screenDetail = x }) <$> f (screenDetail s)

_workspace :: Lens
    (Screen tag layout window screenID screenDimensions)
    (Screen tag' layout' window' screenID screenDimensions)
    (Workspace tag layout window)
    (Workspace tag' layout' window')
_workspace f s = (\ x -> s{ workspace = x }) <$> f (workspace s)


--- Workspace Lenses:

_layout :: Lens
    (Workspace tag layout window) (Workspace tag layout' window)
    layout layout'
_layout f s = (\ x -> s{ layout = x }) <$> f (layout s)

_stack :: Lens
    (Workspace tag layout window) (Workspace tag layout window')
    (Maybe (Stack window)) (Maybe (Stack window'))
_stack f s = (\ x -> s{ stack = x }) <$> f (stack s)

_tag :: Lens
    (Workspace workspaceID layout window)
    (Workspace workspaceID' layout window)
    workspaceID
    workspaceID'
_tag f s = (\ x -> s{ tag = x }) <$> f (tag s)
