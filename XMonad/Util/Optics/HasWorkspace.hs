{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module XMonad.Util.Optics.HasWorkspace where

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
import XMonad.StackSet (Screen, StackSet, Workspace, RationalRect)
import qualified XMonad.Util.Optics as O
import XMonad.Util.Optics.Types


class
    ( HasLayouts tw tw (ALayoutOf w) (ALayoutOf w)
    , HasLayouts tw' tw' (ALayoutOf w') (ALayoutOf w')
    ) =>
    HasWorkspaces tw tw' w w'
    | tw -> w, tw' -> w', tw w' -> tw', tw' w -> tw
  where
    _workspaces :: Traversal tw tw' w w'
    default _workspaces :: HasWorkspace tw tw' w w' => Traversal tw tw' w w'
    _workspaces = _workspace

class
    ( HasWorkspaces tw tw' w w'
    , HasTag tw tw (WorkspaceIdOf w) (WorkspaceIdOf w)
    , HasTag tw' tw' (WorkspaceIdOf w') (WorkspaceIdOf w')
    , HasLayout tw tw (ALayoutOf w) (ALayoutOf w)
    , HasLayout tw' tw' (ALayoutOf w') (ALayoutOf w')
    ) =>
    HasWorkspace tw tw' w w'
    | tw -> w, tw' -> w', tw w' -> tw', tw' w -> tw
  where
    _workspace :: Lens tw tw' w w'

class HasTag ti ti' i i'
    | ti -> i, ti' -> i', ti i' -> ti', ti' i -> ti
  where
    _tag :: Lens ti ti' i i'
    default _tag ::
        HasWorkspace
            ti ti'
            (Workspace i layout window) (Workspace i' layout window) =>
        Lens ti ti' i i'
    _tag = _workspace . O._tag

class HasLayouts tl tl' l l'
    | tl -> l, tl' -> l', tl l' -> tl', tl' l -> tl
  where
    _layouts :: Traversal tl tl' l l'
    default _layouts ::
        HasWorkspaces
            tl tl'
            (Workspace workspaceID l window) (Workspace workspaceID l' window) =>
        Traversal tl tl' l l'
    _layouts = _workspaces . O._layout

class HasLayout tl tl' l l'
    | tl -> l, tl' -> l', tl l' -> tl', tl' l -> tl
  where
    _layout :: Lens tl tl' l l'
    default _layout ::
        HasWorkspace
            tl tl'
            (Workspace workspaceID l window) (Workspace workspaceID l' window) =>
        Lens tl tl' l l'
    _layout = _workspace . O._layout


------------------------------------------------------------------------------
--- Instances

--- Workspace instances

instance HasWorkspace
    (Workspace workspaceID layout window) (Workspace workspaceID' layout' window')
    (Workspace workspaceID layout window) (Workspace workspaceID' layout' window')
  where
    _workspace = id

instance HasWorkspaces
    (Workspace workspaceID layout window) (Workspace workspaceID' layout' window')
    (Workspace workspaceID layout window) (Workspace workspaceID' layout' window')

instance HasTag
    (Workspace workspaceID layout window) (Workspace workspaceID' layout window)
    workspaceID workspaceID'

instance HasLayouts
    (Workspace workspaceID layout window) (Workspace workspaceID layout' window)
    layout layout'

instance HasLayout
    (Workspace workspaceID layout window) (Workspace workspaceID layout' window)
    layout layout'


--- Screen instances

instance HasWorkspace
    (Screen workspaceID layout window screenID screenDimensions)
    (Screen workspaceID' layout' window' screenID screenDimensions)
    (Workspace workspaceID layout window)
    (Workspace workspaceID' layout' window')
  where
    _workspace = O._workspace

instance HasWorkspaces
    (Screen workspaceID layout window screenID screenDimensions)
    (Screen workspaceID' layout' window' screenID screenDimensions)
    (Workspace workspaceID layout window)
    (Workspace workspaceID' layout' window')

instance HasTag
    (Screen workspaceID layout window screenID screenDimensions)
    (Screen workspaceID' layout window screenID screenDimensions)
    workspaceID
    workspaceID'

instance HasLayouts
    (Screen workspaceID layout window screenID screenDimensions)
    (Screen workspaceID layout' window screenID screenDimensions)
    layout
    layout'

instance HasLayout
    (Screen workspaceID layout window screenID screenDimensions)
    (Screen workspaceID layout' window screenID screenDimensions)
    layout
    layout'


--- StackSet
instance HasWorkspaces
    (StackSet workspaceID layout window screenID screenDimensions)
    (StackSet workspaceID' layout' window screenID screenDimensions)
    (Workspace workspaceID layout window)
    (Workspace workspaceID' layout' window)
  where
    _workspaces = O._workspaces

instance HasLayouts
    (StackSet workspaceID layout window screenID screenDimensions)
    (StackSet workspaceID layout' window screenID screenDimensions)
    layout
    layout'
