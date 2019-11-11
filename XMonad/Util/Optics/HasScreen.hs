{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module XMonad.Util.Optics.HasScreen where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Semigroup (All)
import Data.Set (Set)

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
import Graphics.X11.Xlib.Extras (Event)

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
import XMonad.StackSet (RationalRect, Screen, StackSet)
import qualified XMonad.Util.Optics as O
import XMonad.Util.Optics.Types


class
    ( HasScreenId ts ts (ScreenIdOf s) (ScreenIdOf s)
    , HasScreenId ts' ts' (ScreenIdOf s') (ScreenIdOf s')
    , HasScreenDetail ts ts (ScreenDetailOf s) (ScreenDetailOf s)
    , HasScreenDetail ts' ts' (ScreenDetailOf s') (ScreenDetailOf s')
    ) =>
    HasScreen ts ts' s s'
    | ts -> s, ts' -> s', ts s' -> ts', ts' s -> ts
  where
    _screen :: Lens ts ts' s s'

class
    (ScreenIdOf ti ~ i, ScreenIdOf ti' ~ i') =>
    HasScreenId ti ti' i i'
    | ti -> i, ti' -> i', ti i' -> ti', ti' i -> ti
  where
    _screenId :: Lens ti ti' i i'
    default _screenId ::
        HasScreen
            ti
            ti'
            (Screen workspaceID layout window i screenDimensions)
            (Screen workspaceID layout window i' screenDimensions) =>
        Lens ti ti' i i'
    _screenId = _screen . O._screenId

class
    (ScreenDetailOf td ~ d, ScreenDetailOf td' ~ d') =>
    HasScreenDetail td td' d d'
    | td -> d, td' -> d', td d' -> td', td' d -> td
  where
    _screenDetail :: Lens td td' d d'
    default _screenDetail ::
        HasScreen
            td
            td'
            (Screen workspaceID layout window screenID d)
            (Screen workspaceID layout window screenID d') =>
        Lens td td' d d'
    _screenDetail = _screen . O._screenDetail

                     ------------------------------------------------------------------------------
--- Instances:

--- Screen:

instance HasScreen
    (Screen workspaceID layout window screenID screenDimensions)
    (Screen workspaceID' layout' window' screenID' screenDimensions')
    (Screen workspaceID layout window screenID screenDimensions)
    (Screen workspaceID' layout' window' screenID' screenDimensions')
  where
    _screen = id

instance HasScreenId
    (Screen workspaceID layout window screenID screenDimensions)
    (Screen workspaceID layout window screenID' screenDimensions)
    screenID
    screenID'

instance HasScreenDetail
    (Screen workspaceID layout window screenID screenDimensions)
    (Screen workspaceID layout window screenID screenDimensions')
    screenDimensions
    screenDimensions'

--- ScreenId:
instance HasScreenId ScreenId ScreenId ScreenId ScreenId where
    _screenId = id

--- ScreenDetail:
instance HasScreenDetail ScreenDetail ScreenDetail ScreenDetail ScreenDetail where
    _screenDetail = id
