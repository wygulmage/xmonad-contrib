{-# LANGUAGE DefaultSignatures #-}

module XMonad.Util.Optics.HasXConf where

import Data.Map (Map)

import Graphics.X11.Xlib
    (Button, Display, KeyMask, KeySym, Pixel, Position, Window)
import Graphics.X11.Xlib.Extras (Event)

import XMonad.Core (X, XConf)
import qualified XMonad.Util.Optics as O
import XMonad.Util.Optics.Types

------------------------------------------------------------------------------
--- Classes
class
    ( HasButtonActions ta
    , HasCurrentEvent ta
    , HasDisplay ta
    , HasBorder ta
    , HasTheRoot ta
    -- , HasXConfig ta ta Layout Layout
    ) =>
    HasXConf ta where
    _xConf :: Simple Lens ta XConf

class HasBorder ta where
    _focusedBorder, _normalBorder :: Simple Lens ta Pixel

    default _focusedBorder :: HasXConf ta => Simple Lens ta Pixel
    _focusedBorder = _xConf . O._focusedBorder

    default _normalBorder :: HasXConf ta => Simple Lens ta Pixel
    _normalBorder = _xConf . O._normalBorder

class HasButtonActions ta where
    _buttonActions :: Simple Lens ta (Map (KeyMask, Button) (Window -> X ()))
    default _buttonActions ::
        HasXConf ta => Simple Lens ta (Map (KeyMask, Button) (Window -> X ()))
    _buttonActions = _xConf . O._buttonActions

class HasCurrentEvent ta where
    _currentEvent :: Simple Lens ta (Maybe Event)
    default _currentEvent :: HasXConf ta => Simple Lens ta (Maybe Event)
    _currentEvent = _xConf . O._currentEvent

class HasDisplay ta where
    _display :: Simple Lens ta Display
    default _display :: HasXConf ta => Simple Lens ta Display
    _display = _xConf . O._display

class HasKeyActions ta where
    _keyActions :: Simple Lens ta (Map (KeyMask, KeySym) (X ()))
    default _keyActions ::
        HasXConf ta => Simple Lens ta (Map (KeyMask, KeySym) (X ()))
    _keyActions = _xConf . O._keyActions

class HasMouseFocused ta where
    _mouseFocused :: Simple Lens ta Bool
    default _mouseFocused :: HasXConf ta => Simple Lens ta Bool
    _mouseFocused = _xConf . O._mouseFocused

class HasMousePosition ta where
    _mousePosition :: Simple Lens ta (Maybe (Position, Position))
    default _mousePosition :: HasXConf ta => Simple Lens ta (Maybe (Position, Position))
    _mousePosition = _xConf . O._mousePosition

class HasTheRoot ta where
    _theRoot :: Simple Lens ta Window
    default _theRoot :: HasXConf ta => Simple Lens ta Window
    _theRoot = _xConf . O._theRoot


------------------------------------------------------------------------------
--- Instances

instance HasXConf XConf where _xConf = id
instance HasBorder XConf
instance HasButtonActions XConf
instance HasCurrentEvent XConf
instance HasDisplay XConf
instance HasKeyActions XConf
instance HasMouseFocused XConf
instance HasMousePosition XConf
instance HasTheRoot XConf
