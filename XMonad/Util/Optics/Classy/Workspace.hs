{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LiberalTypeSynonyms    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module XMonad.Util.Optics.Classy.Workspace
  where

--- Classes:
import Control.Category (id, (.))
import Data.Functor ((<$>))
import Data.Traversable (traverse)
import XMonad.Util.Optics.Classy.Stack

--- Types:
import Data.Map (Map)
import Data.Semigroup (All)
import Data.Set (Set)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Graphics.X11.Xlib
    (Button, ButtonMask, Display, KeyMask, KeySym, Pixel, Position, Window)
import Graphics.X11.Xlib.Extras (Event)
import Prelude (Bool, Either, Int, Maybe, String)
import XMonad.Core
    (Layout, ManageHook, ScreenDetail, ScreenId, StateExtension, WindowSet, WindowSpace, WorkspaceId, X, XConf, XConfig, XState)
import XMonad.StackSet (RationalRect, Screen, Stack, StackSet, Workspace)
import XMonad.Util.Optics.Types
    (ALayoutOf, Lens, ScreenDetailOf, ScreenIdOf, ScreenOf, Simple, Traversal, WindowOf, WorkspaceIdOf, WorkspaceOf)

--- Functions:
import qualified XMonad.Util.Optics as O

class
    (a ~ ALayoutOf ta, b ~ ALayoutOf tb) =>
    HasLayouts ta tb a b
    | ta -> a, tb -> b
    , ta b -> tb, tb a -> ta
  where
    _layouts :: Traversal ta tb a b
    default _layouts :: HasLayout ta tb a b => Traversal ta tb a b
    _layouts = _layout

class
    (HasLayouts tl tl' l l') =>
    HasLayout tl tl' l l'
    -- | ta -> a, tb -> b -- This is also given by the constraints above.
    -- , ta b -> tb, tb a -> ta
  where
    _layout :: Lens tl tl' l l'
    default _layout :: HasWorkspace
        tl tl'
        (Workspace workspaceID l window) (Workspace workspaceID l' window)
        =>
        Lens tl tl' l l'
    _layout = _workspace . O._layout


class
    -- (a ~ WorkspaceIdOf ta, b ~ WorkspaceIdOf tb) =>
    HasTags tt tt' t t'
    | tt -> t, tt' -> t', tt t' -> tt', tt' t -> tt
  where
    _tags :: Traversal tt tt' t t'
    default _tags :: HasTag tt tt' t t' => Traversal tt tt' t t'
    _tags = _tag

class
    (HasTags tt tt' t t') =>
    HasTag tt tt' t t'
    -- | ta -> a, tb -> b
    -- , ta b -> tb, tb a -> ta
  where
    _tag :: Lens tt tt' t t'
    default _tag :: HasWorkspace tt tt' (Workspace t layout window) (Workspace t' layout window) => Lens tt tt' t t'
    _tag = _workspace . O._tag
    -- Should be `Simple Lens ta WorkspaceId`

-- HasStack might be better named 'MayHaveStack'.
class
    HasStack ta tb a b | ta -> a, tb -> b, ta b -> tb, tb a -> ta where
    _stack :: Lens ta tb (Maybe (Stack a)) (Maybe (Stack b))

class
    -- (a ~ WorkspaceOf ta, b ~ WorkspaceOf tb) =>
    HasWorkspaces tw tw' w w'
    | tw -> w, tw' -> w', tw w' -> tw', tw' w -> tw
  where
    _workspaces :: Traversal tw tw' w w'
    default _workspaces :: HasWorkspace tw tw' w w' => Traversal tw tw' w w'
    _workspaces = _workspace

class HasWorkspaces ta tb a b => HasWorkspace ta tb a b where
    _workspace :: Lens ta tb a b

--- Workspace instances:

instance HasWorkspaces
    (Workspace tag layout window) (Workspace tag' layout' window')
    (Workspace tag layout window) (Workspace tag' layout' window')
instance HasWorkspace
    (Workspace tag layout window) (Workspace tag' layout' window')
    (Workspace tag layout window) (Workspace tag' layout' window')
  where
    _workspace = id

instance
    HasLayout
    (Workspace tag layout window) (Workspace tag layout' window)
    layout layout'
  where
    _layout = O._layout

instance HasStack
    (Workspace tag layout window) (Workspace tag layout window')
    window window'
  where
    _stack = O._stack

instance HasTags
    (Workspace workspaceID layout window)
    (Workspace workspaceID' layout window)
    workspaceID
    workspaceID'

instance HasTag
    (Workspace workspaceID layout window)
    (Workspace workspaceID' layout window)
    workspaceID
    workspaceID'
  -- where
    -- _tag = O._tag

instance HasLayouts
    (Workspace tag layout window) (Workspace tag layout' window)
    layout layout'
  where
    _layouts = _layout
