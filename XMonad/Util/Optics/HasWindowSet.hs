{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module XMonad.Util.Optics.HasWindowSet where

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
import XMonad.StackSet (RationalRect)
import qualified XMonad.Util.Optics as O
import XMonad.Util.Optics.Types


class
    ( HasVisible tw tw' (ScreenOf w) (ScreenOf w')
    , HasHidden tw (WorkspaceOf w), HasHidden tw' (WorkspaceOf w')
    , HasFloating tw (WindowOf w), HasFloating tw' (WindowOf w')
    ) =>
    HasWindowSet tw tw' w w'
    | tw -> w, tw' -> w', tw w' -> tw', tw' w -> tw
  where
    _windowset :: Lens tw tw' w w'

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


class HasFloating tw w | tw -> w where
    _floating :: Simple Lens tw (Map w RationalRect)

class
    -- (HasWorkspaces ta ta (WorkspaceOf ta) (WorkspaceOf ta)) =>
    (ws ~ WorkspaceOf tws) =>
    HasHidden tws ws
  where
    _hidden :: Simple Lens tws [ws]
