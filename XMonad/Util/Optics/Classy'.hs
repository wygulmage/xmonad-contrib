{-# LANGUAGE DefaultSignatures #-}

module XMonad.Util.Optics.Classy' where

import Data.Map (Map)
import Data.Semigroup (All)
import Data.Set (Set)

import Graphics.X11.Xlib
    (Button, ButtonMask, Display, KeyMask, KeySym, Pixel, Position, Window)

import qualified XMonad.Util.Optics as O
import XMonad.Util.Optics.Types
import XMonad.Core
    (Layout, ManageHook, ScreenDetail, ScreenId, StateExtension, WindowSet, WindowSpace, WorkspaceId, X, XConf, XConfig, XState)

------------------------------------------------------------------------------
--- Classes

class
    ( HasMapped a
    , HasDragging a
    , HasExtensibleState a
    , HasNumberlockMask a
    , HasExtensibleState a
    ) =>
    HasXState a where
    _xState :: Simple Lens a XState


class HasMapped a where

    _mapped :: Simple Lens a (Set Window)
    default _mapped :: HasXState a => Simple Lens a (Set Window)
    _mapped = _xState . O._mapped

    _waitingUnmap :: Simple Lens a (Map Window Int)
    default _waitingUnmap :: HasXState a => Simple Lens a (Map Window Int)
    _waitingUnmap = _xState . O._waitingUnmap


class HasDragging a where
    _dragging :: Simple Lens a (Maybe (Position -> Position -> X (), X ()))
    default _dragging ::
        HasXState a =>
        Simple Lens a (Maybe (Position -> Position -> X (), X ()))
    _dragging = _xState . O._dragging


class HasExtensibleState a where
    _extensibleState ::
        Simple Lens a (Map String (Either String StateExtension))
    default _extensibleState ::
        HasXState a =>
        Simple Lens a (Map String (Either String StateExtension))
    _extensibleState = _xState . O._extensibleState


class HasNumberlockMask a where
    _numberlockMask :: Simple Lens a KeyMask
    default _numberlockMask :: HasXState a => Simple Lens a KeyMask
    _numberlockMask = _xState . O._numberlockMask

------------------------------------------------------------------------------
--- Instances

instance HasXState XState where _xState = id
instance HasDragging XState
instance HasNumberlockMask XState
instance HasExtensibleState XState
instance HasMapped XState
