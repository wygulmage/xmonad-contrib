{-# LANGUAGE
    NoImplicitPrelude
  , RankNTypes
  , TypeFamilies
  #-}

module XMonad.Util.Optics.Types where

import Control.Applicative (Applicative)
import Data.Functor (Functor)

import Graphics.X11.Xlib (Window)
import XMonad.Core (Layout, ScreenDetail, ScreenId, WindowSet, WorkspaceId, XConf, XConfig, XState)
import XMonad.StackSet (Screen, Stack, StackSet, Workspace)


------- Optical Types -------
type LensLike m ta tb a b = (a -> m b) -> ta -> m tb
type Lens ta tb a b = forall m. Functor m => LensLike m ta tb a b
type Traversal ta tb a b = forall m. Applicative m => LensLike m ta tb a b
type Simple o ta a = o ta ta a a


------- Type Families for XMonad.StackSet -------
-- StackSet uses types like `Screen i l a s sd` when it should probably use monomorphic types like `Screen` with i, l, a, s, and sd instantiated as WorkspaceId, Layout Window, Window, ScreenId, and ScreenDetail, respectively.
-- As a result, the classy lenses use functional dependencies and it's all really ugly and too permissive. The type families below are a sort of stopgap to say what should be what and what their relations are to each other, without breaking existing code.

type ScreenOf a = Screen (WorkspaceIdOf a) (LayoutOf a) (WindowOf a) (ScreenIdOf a) (ScreenDetailOf a)

type WorkspaceOf a = Workspace (WorkspaceIdOf a) (LayoutOf a) (WindowOf a)

type family LayoutOf a
type instance LayoutOf (Layout window) = Layout window
type instance LayoutOf (Workspace tag layout window) = layout
type instance LayoutOf (Screen tag layout window screenID screenDimensions)
    = layout
type instance LayoutOf (StackSet tag layout window screenID screenDimensions)
    = layout
type instance LayoutOf (XConfig l) = l Window
type instance LayoutOf XConf = Layout Window
type instance LayoutOf XState = Layout Window

type family WorkspaceIdOf a
type instance WorkspaceIdOf WorkspaceId = WorkspaceId
type instance WorkspaceIdOf (Workspace tag layout window) = tag
type instance WorkspaceIdOf (Screen tag layout window screenID screenDimensions)
    = tag
type instance WorkspaceIdOf (StackSet tag layout window screenID screenDimensions)
    = tag
type instance WorkspaceIdOf (XConfig l) = WorkspaceId
type instance WorkspaceIdOf XConf = WorkspaceId
type instance WorkspaceIdOf XState = WorkspaceId

type family WindowOf a
type instance WindowOf Window = Window
type instance WindowOf (Stack window) = window
type instance WindowOf (Workspace tag layout window) = window
type instance WindowOf (Screen tag layout window screenID screenDimensions)
    = window
type instance WindowOf (StackSet tag layout window screenID screenDimensions)
    = window
type instance WindowOf (XConfig l) = Window
type instance WindowOf XConf = Window
type instance WindowOf XState = Window

type family ScreenIdOf a
type instance ScreenIdOf (ScreenId) = ScreenId
type instance ScreenIdOf (Screen tag layout window screenID screenDimensions)
    = screenID
type instance ScreenIdOf (StackSet tag layout window screenID screenDimensions)
    = screenID
type instance ScreenIdOf (XConfig l) = ScreenId
type instance ScreenIdOf XConf = ScreenId
type instance ScreenIdOf XState = ScreenId

type family ScreenDetailOf a
type instance ScreenDetailOf ScreenDetail = ScreenDetail
type instance ScreenDetailOf (Screen tag layout window screenID screenDimensions)
    = screenDimensions
type instance ScreenDetailOf (StackSet tag layout window screenID screenDimensions)
    = screenDimensions
type instance ScreenDetailOf XState = ScreenDetail
