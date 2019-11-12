{- |
Description: Lenses and Traversals for XMonad types

Copyright: © 2019 Keith Wygant
License:   Public Domain

Maintainer:  Keith Wygant
Stability:   unstable
Portability: unportable


Designed to be used with an optics library like lens or microlens (but the lenses are useful on their own as traversals)
-}

module XMonad.Util.Optics

{-

Examples with functions from lens or microlens
----------------------------------------------

Map a function `f` over the current stack if it isn't empty (equivalent to `modify'`):
`_index %~ f`

Map a function `f` over the `Workspace`s in a `StackSet` (equivalent to `mapWorkspace`):
`_workspaces %~ f`

Get a list of all `Workspace`s in a `StackSet` (equivalent to `workspaces`):
`toListOf _workspaces`

Map a function `f` over the layouts in a `StackSet` (equivalent to `mapLayout`):
`_workspaces . _layout %~ f`

Equivalent to StackSet.with z f:
`views (_current . _workspace . _stack) (maybe z f)` (also equivalent to `ask (with z f)`)
or `maybe z f . view (current . _workspace . _stack)`
Using `XMonad.Util.Optics.Classy`, `views (_current . _stack) (maybe z f)` suffices.

-}

-- * XConf Lenses
    ( _buttonActions
    , _xConfig
    , _currentEvent
    , _display
    , _focusedBorder, _normalBorder
    , _keyActions
    , _mouseFocused
    , _mousePosition
    , _theRoot
-- * XConfig Lenses
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
-- * XState Lenses
    , _dragging
    , _extensibleState
    , _mapped
    , _waitingUnmap
    , _numberlockMask
    , _windowset
-- * StackSet (WindowSet) Optics
    , _current
    , _floating
    , _screens
    , _visible
    , _hidden
    , _workspaces
    , _index
-- * Screen Lenses
    , _screenId
    , _screenDetail
    , _workspace
-- * Workspace Lenses
    , _layout
    , _stack
    , _tag
-- * Stack (Zipper) Optics
    , _focus
    , _up
    , _down
    , traverseStack
    )
  where

import XMonad.Util.Optics.Concrete
