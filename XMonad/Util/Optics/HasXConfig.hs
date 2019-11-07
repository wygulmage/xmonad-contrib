{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module XMonad.Util.Optics.HasXConfig where

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
    , WindowSpace
    , WorkspaceId
    , X
    , XConf
    , XConfig
    , XState
    )
import qualified XMonad.Util.Optics as O
import XMonad.Util.Optics.Types

------------------------------------------------------------------------------
--- Classes
class
     ( HasBorderColor tl, HasBorderColor tl'
     , HasBorderWidth tl, HasBorderWidth tl'
     , HasClickJustFocuses tl, HasClickJustFocuses tl'
     , HasClientMask tl, HasClientMask tl'
     , HasFocusFollowsMouse tl, HasFocusFollowsMouse tl'
     , HasHandleEventHook tl, HasHandleEventHook tl'
     , HasHandleExtraArgs tl, HasHandleExtraArgs tl'
     , HasKeys tl, HasKeys tl'
     , HasLayoutHook tl tl' l l'
     , HasLogHook tl, HasLogHook tl'
     , HasManageHook tl, HasManageHook tl'
     , HasModMask tl, HasModMask tl'
     , HasMouseBindings tl, HasMouseBindings tl'
     , HasRootMask tl, HasRootMask tl'
     , HasStartupHook tl, HasStartupHook tl'
     , HasTerminal tl, HasTerminal tl'
     , HasWorkspaceNames tl, HasWorkspaceNames tl'
     ) =>
     HasXConfig tl tl' l l'
     | tl -> l, tl' -> l', tl l' -> tl', tl' l -> tl
  where
    _xConfig :: Lens tl tl' (XConfig l) (XConfig l')
    -- `_xConfig` instead of `_config` to (hopefully) avoid some confusion

class
    HasLayoutHook tl tl' l l'
    | tl -> l, tl' -> l', tl l' -> tl', tl' l -> tl
  where
    _layoutHook :: Lens tl tl' (l Window) (l' Window)
    default _layoutHook ::
        HasXConfig tl tl' l l' => Lens tl tl' (l Window) (l' Window)
    _layoutHook = _xConfig . O._layoutHook


class HasBorderColor ta where
    _focusedBorderColor, _normalBorderColor :: Simple Lens ta String

    default _focusedBorderColor ::
        HasXConfig ta ta l l => Simple Lens ta String
    _focusedBorderColor = _xConfig . O._focusedBorderColor

    default _normalBorderColor ::
        HasXConfig ta ta l l => Simple Lens ta String
    _normalBorderColor = _xConfig . O._normalBorderColor

class HasBorderWidth a where
    _borderWidth :: Simple Lens a Dimension
    default _borderWidth :: HasXConfig a a l l => Simple Lens a Dimension
    _borderWidth = _xConfig . O._borderWidth

class HasClickJustFocuses a where
    _clickJustFocuses :: Simple Lens a Bool
    default _clickJustFocuses :: HasXConfig a a l l => Simple Lens a Bool
    _clickJustFocuses = _xConfig . O._clickJustFocuses

class HasClientMask a where
    _clientMask :: Simple Lens a EventMask
    default _clientMask :: HasXConfig a a l l => Simple Lens a EventMask
    _clientMask = _xConfig . O._clientMask

class HasFocusFollowsMouse a where
    _focusFollowsMouse :: Simple Lens a Bool
    default _focusFollowsMouse :: HasXConfig a a l l => Simple Lens a Bool
    _focusFollowsMouse = _xConfig . O._focusFollowsMouse

class HasHandleEventHook ta where
    _handleEventHook :: Simple Lens ta (Event -> X All)
    default _handleEventHook ::
        HasXConfig ta ta l l => Simple Lens ta (Event -> X All)
    _handleEventHook = _xConfig . O._handleEventHook

class HasHandleExtraArgs ta where
    _handleExtraArgs ::
        Simple Lens ta ([String] -> XConfig Layout -> IO (XConfig Layout))
    default _handleExtraArgs ::
        HasXConfig ta ta l l =>
        Simple Lens ta ([String] -> XConfig Layout -> IO (XConfig Layout))
    _handleExtraArgs = _xConfig . O._handleExtraArgs

class HasKeys ta where
    _keys ::
        Simple Lens ta (XConfig Layout -> Map (ButtonMask, KeySym) (X ()))
    default _keys ::
        HasXConfig ta ta l l =>
        Simple Lens ta (XConfig Layout -> Map (ButtonMask, KeySym) (X ()))
    _keys = _xConfig . O._keys

class HasLogHook a where
    _logHook :: Simple Lens a (X ())
    default _logHook :: HasXConfig a a l l => Simple Lens a (X ())
    _logHook = _xConfig . O._logHook

class HasManageHook ta where
    _manageHook :: Simple Lens ta ManageHook
    default _manageHook :: HasXConfig ta ta l l => Simple Lens ta ManageHook
    _manageHook = _xConfig . O._manageHook

class HasModMask ta where
    _modMask :: Simple Lens ta KeyMask
    default _modMask :: HasXConfig ta ta l l => Simple Lens ta KeyMask
    _modMask = _xConfig . O._modMask

class HasMouseBindings ta where
    _mouseBindings :: Simple Lens
        ta (XConfig Layout -> Map (ButtonMask, Button) (Window -> X ()))
    default _mouseBindings ::
        HasXConfig ta ta l l =>
        Simple Lens
        ta  (XConfig Layout -> Map (ButtonMask, Button) (Window -> X ()))
    _mouseBindings = _xConfig . O._mouseBindings

class HasRootMask ta where
    _rootMask :: Simple Lens ta EventMask
    default _rootMask :: HasXConfig ta ta l l => Simple Lens ta EventMask
    _rootMask = _xConfig . O._rootMask

class HasStartupHook ta where
    _startupHook :: Simple Lens ta (X ())
    default _startupHook :: HasXConfig ta ta l l => Simple Lens ta (X ())
    _startupHook = _xConfig . O._startupHook

class HasTerminal ta where
    _terminal :: Simple Lens ta String
    default _terminal :: HasXConfig ta ta l l => Simple Lens ta String
    _terminal = _xConfig . O._terminal

class HasWorkspaceNames ta where
  -- `_workspaceNames` is used so that `_workspaces` can be the Traversal of Workspaces.
  _workspaceNames :: Simple Lens ta [WorkspaceId]
  default _workspaceNames ::
      HasXConfig ta ta l l => Simple Lens ta [WorkspaceId]
  _workspaceNames = _xConfig . O._workspaceNames

------------------------------------------------------------------------------
--- Instances

instance HasXConfig (XConfig layout) (XConfig layout') layout layout' where
    _xConfig = id

instance HasLayoutHook (XConfig layout) (XConfig layout') layout layout'
instance HasBorderColor (XConfig layout)
instance HasBorderWidth (XConfig layout)
instance HasClickJustFocuses (XConfig layout)
instance HasClientMask (XConfig layout)
instance HasFocusFollowsMouse (XConfig layout)
instance HasHandleEventHook (XConfig layout)
instance HasHandleExtraArgs (XConfig layout)
instance HasKeys (XConfig layout)
instance HasLogHook (XConfig layout)
instance HasManageHook (XConfig layout)
instance HasModMask (XConfig layout)
instance HasMouseBindings (XConfig layout)
instance HasRootMask (XConfig layout)
instance HasStartupHook (XConfig layout)
instance HasTerminal (XConfig layout)
instance HasWorkspaceNames (XConfig layout)

instance HasXConfig XConf XConf Layout Layout where
    _xConfig = O._xConfig

instance HasLayoutHook XConf XConf Layout Layout
instance HasBorderColor XConf
instance HasBorderWidth XConf
instance HasClickJustFocuses XConf
instance HasClientMask XConf
instance HasFocusFollowsMouse XConf
instance HasHandleEventHook XConf
instance HasHandleExtraArgs XConf
instance HasKeys XConf
instance HasLogHook XConf
instance HasManageHook XConf
instance HasModMask XConf
instance HasMouseBindings XConf
instance HasRootMask XConf
instance HasStartupHook XConf
instance HasTerminal XConf
instance HasWorkspaceNames XConf
