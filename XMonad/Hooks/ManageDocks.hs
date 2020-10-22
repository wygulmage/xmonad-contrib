{-# LANGUAGE DeriveDataTypeable, PatternGuards, FlexibleInstances, MultiParamTypeClasses, BangPatterns, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Hooks.ManageDocks
-- Copyright    : (c) Joachim Breitner <mail@joachim-breitner.de>
-- License      : BSD
--
-- Maintainer   : Joachim Breitner <mail@joachim-breitner.de>
-- Stability    : unstable
-- Portability  : unportable
--
-- This module provides tools to automatically manage 'dock' type programs,
-- such as gnome-panel, kicker, dzen, and xmobar.

module XMonad.Hooks.ManageDocks (
    -- * Usage
    -- $usage
    docks, manageDocks, checkDock, AvoidStruts(..), avoidStruts, avoidStrutsOn,
    docksEventHook, docksStartupHook,
    ToggleStruts(..),
    SetStruts(..),
    module XMonad.Util.Types,

#ifdef TESTING
    r2c,
    c2r,
    RectC(..),
#endif

    -- for XMonad.Actions.FloatSnap
    calcGap
    ) where


-----------------------------------------------------------------------------
import XMonad
import Foreign.C.Types (CLong)
import XMonad.Layout.LayoutModifier
import XMonad.Util.Types
import XMonad.Util.WindowProperties (getProp32s)
import XMonad.Util.XUtils (fi)
import qualified XMonad.Util.ExtensibleState as XS

import Data.Monoid (All(..))
import Data.Functor ((<$>), (<$))
import Data.Foldable (for_)

import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import Control.Monad (when)

-- $usage
-- To use this module, add the following import to @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.ManageDocks
--
-- Wrap your xmonad config with a call to 'docks', like so:
--
-- > main = xmonad $ docks def
--
-- Then add 'avoidStruts' or 'avoidStrutsOn' layout modifier to your layout
-- to prevent windows from overlapping these windows.
--
-- > layoutHook = avoidStruts (tall ||| mirror tall ||| ...)
-- >                   where  tall = Tall 1 (3/100) (1/2)
--
-- 'AvoidStruts' also supports toggling the dock gaps; add a keybinding
-- similar to:
--
-- > ,((modm, xK_b     ), sendMessage ToggleStruts)
--
-- If you have multiple docks, you can toggle their gaps individually.
-- For example, to toggle only the top gap:
--
-- > ,((modm .|. controlMask, xK_t), sendMessage $ ToggleStrut U)
--
-- Similarly, you can use 'D', 'L', and 'R' to individually toggle
-- gaps on the bottom, left, or right.
--
-- If you want certain docks to be avoided but others to be covered by
-- default, you can manually specify the sides of the screen on which
-- docks should be avoided, using 'avoidStrutsOn'.  For example:
--
-- > layoutHook = avoidStrutsOn [U,L] (tall ||| mirror tall ||| ...)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".
--

-- | Add docks functionality to the given config.  See above for an example.
docks :: XConfig a -> XConfig a
docks c = c { startupHook     = docksStartupHook <+> startupHook c
            , handleEventHook = docksEventHook <+> handleEventHook c
            , manageHook      = manageDocks <+> manageHook c }

newtype Cache = Cache (Map.Map Window Struts)
  deriving (Eq, Typeable)

getCache :: Cache -> Map.Map Window Struts
getCache (Cache c) = c

data UpdateDocks = UpdateDocks deriving Typeable
instance Message UpdateDocks

refreshDocks :: X ()
refreshDocks = sendMessage UpdateDocks

instance ExtensionClass Cache where
  initialValue = Cache Map.empty

cacheStruts :: Window -> Struts -> X Bool
cacheStruts w strut =
    XS.modified (Cache . Map.insert w strut . getCache)

uncacheStruts :: Window -> X Bool
uncacheStruts w = XS.modified (Cache . Map.delete w . getCache)

-- | Detects if the given window is of type DOCK and if so, reveals
--   it, but does not manage it.
manageDocks :: ManageHook
manageDocks = checkDock --> (doIgnore <+> setDocksMask)
  where
    setDocksMask = do
        ask >>= \win -> liftX $ withDisplay $ \dpy ->
            io $ selectInput dpy win (propertyChangeMask .|. structureNotifyMask)
        mempty

-- | Checks if a window is a DOCK or DESKTOP window
checkDock :: Query Bool
checkDock = ask >>= \w -> liftX $ do
    dock <- getAtom "_NET_WM_WINDOW_TYPE_DOCK"
    desk <- getAtom "_NET_WM_WINDOW_TYPE_DESKTOP"
    mbr <- getProp32s "_NET_WM_WINDOW_TYPE" w
    return $! maybe False (any ((`elem` [dock,desk]) . fromIntegral)) mbr

-- | Whenever a new dock appears, refresh the layout immediately to avoid the
-- new dock.
docksEventHook :: Event -> X All
docksEventHook event = case event of
    MapNotifyEvent { ev_window = w } -> do
        whenX (runQuery checkDock w <&&> (not <$> isClient w)) $ do
            strut <- getStruts w
            whenX (cacheStruts w strut) refreshDocks
        return (All True)
    PropertyEvent { ev_window = w, ev_atom = a } -> do
        nws <- getAtom "_NET_WM_STRUT"
        nwsp <- getAtom "_NET_WM_STRUT_PARTIAL"
        when (a == nws || a == nwsp) $ do
           strut <- getStruts w
           whenX (cacheStruts w strut) refreshDocks
        return (All True)
    DestroyWindowEvent {ev_window = w} -> do
        whenX (uncacheStruts w) refreshDocks
        return (All True)
    _ -> return (All True)

docksStartupHook :: X ()
docksStartupHook = withDisplay $ \dpy -> do
    rootw <- asks theRoot
    (_, _ , wins) <- io $ queryTree dpy rootw
    for_ wins $ \ w ->
        whenX (runQuery checkDock w) $
            () <$ cacheStruts w <$> getStruts w
    refreshDocks

-- | Get the STRUT config, if present, in xmonad gap order.
getStruts :: Window -> X Struts
getStruts w = do
    msp <- getProp32s "_NET_WM_STRUT_PARTIAL" w
    case msp of
       Just sp -> return (parseStrutPartial sp)
       Nothing -> maybe noStruts parseStrut <$> getProp32s "_NET_WM_STRUT" w
  where
    parseStrut :: [CLong] -> Struts
    parseStrut (l : r : t : b : []) =
        Struts (fullStrut l) (fullStrut r) (fullStrut t) (fullStrut b)
    parseStrut _ = noStruts

    parseStrutPartial :: [CLong] -> Struts
    parseStrutPartial
        [ l_thick   , r_thick   , t_thick   , b_thick
        , ly1 , ly2 , ry1 , ry2 , tx1 , tx2 , bx1 , bx2 ] =
            Struts
                (mkStrut l_thick ly1 ly2)
                (mkStrut r_thick ry1 ry2)
                (mkStrut t_thick tx1 tx2)
                (mkStrut b_thick bx1 bx2)
    parseStrutPartial _ = noStruts

    mkStrut :: CLong -> CLong -> CLong -> StrutD
    mkStrut thic beg end
        | thic == 0 = noStrut
        | otherwise = StrutD thic beg end

    fullStrut :: CLong -> StrutD
    fullStrut thic = StrutD thic minBound maxBound

-- | Goes through the list of windows and find the gap so that all
--   STRUT settings are satisfied.
calcGap :: S.Set Direction2D -> X (Rectangle -> Rectangle)
calcGap ss = withDisplay $ \dpy -> do
    rootw <- asks theRoot
    struts <- XS.gets (fmap careAbout . Map.elems . getCache)

    -- we grab the window attributes of the root window rather than checking
    -- the width of the screen because xlib caches this info and it tends to
    -- be incorrect after RAndR
    wa <- io $ getWindowAttributes dpy rootw
    let screen = r2c $ Rectangle (fi $ wa_x wa) (fi $ wa_y wa) (fi $ wa_width wa) (fi $ wa_height wa)
    return $ \r -> c2r $ foldr (reduce screen) (r2c r) struts
  where
    careAbout (Struts ml mr mt mb) =
      Struts (care L ml) (care R mr) (care U mt) (care D mb)
      where
        care pos mx
            | S.member pos ss = mx
            | otherwise       = noStrut


-- | Adjust layout automagically: don't cover up any docks, status
--   bars, etc.
avoidStruts :: LayoutClass l a => l a -> ModifiedLayout AvoidStruts l a
avoidStruts = avoidStrutsOn [U,D,L,R]

-- | Adjust layout automagically: don't cover up docks, status bars,
--   etc. on the indicated sides of the screen.  Valid sides are U
--   (top), D (bottom), R (right), or L (left).
avoidStrutsOn :: LayoutClass l a =>
                 [Direction2D]
              -> l a
              -> ModifiedLayout AvoidStruts l a
avoidStrutsOn ss = ModifiedLayout $ AvoidStruts (S.fromList ss)

newtype AvoidStruts a = AvoidStruts (S.Set Direction2D) deriving (Read, Show)

-- | Message type which can be sent to an 'AvoidStruts' layout
--   modifier to alter its behavior.
data ToggleStruts = ToggleStruts
                  | ToggleStrut Direction2D
  deriving (Read, Show, Typeable)

instance Message ToggleStruts

-- | SetStruts is a message constructor used to set or unset specific struts,
-- regardless of whether or not the struts were originally set. Here are some
-- example bindings:
--
-- Show all gaps:
--
-- >   ,((modm .|. shiftMask  ,xK_b),sendMessage $ SetStruts [minBound .. maxBound] [])
--
-- Hide all gaps:
--
-- >   ,((modm .|. controlMask,xK_b),sendMessage $ SetStruts [] [minBound .. maxBound])
--
-- Show only upper and left gaps:
--
-- >   ,((modm .|. controlMask .|. shiftMask,xK_b),sendMessage $ SetStruts [U,L] [minBound .. maxBound])
--
-- Hide the bottom keeping whatever the other values were:
--
-- >   ,((modm .|. controlMask .|. shiftMask,xK_g),sendMessage $ SetStruts [] [D])
data SetStruts = SetStruts { addedStruts   :: [Direction2D]
                           , removedStruts :: [Direction2D] -- ^ These are removed from the currently set struts before 'addedStruts' are added.
                           }
  deriving (Read,Show,Typeable)

instance Message SetStruts

instance LayoutModifier AvoidStruts a where
    modifyLayout (AvoidStruts ss) w r = do
        srect <- fmap ($ r) (calcGap ss)
        -- Ensure _NET_WORKAREA is not set.
        -- See: https://github.com/xmonad/xmonad-contrib/pull/79
        rmWorkarea
        runLayout w srect

    pureMess as@(AvoidStruts ss) m
        | Just ToggleStruts    <- fromMessage m = Just $ AvoidStruts (toggleAll ss)
        | Just (ToggleStrut s) <- fromMessage m = Just $ AvoidStruts (toggleOne s ss)
        | Just (SetStruts n k) <- fromMessage m
        , let newSS = S.fromList n `S.union` (ss S.\\ S.fromList k)
        , newSS /= ss = Just $ AvoidStruts newSS
        | Just UpdateDocks <- fromMessage m = Just as
        | otherwise = Nothing
      where
        toggleAll :: S.Set Direction2D -> S.Set Direction2D
        toggleAll x | S.null x  = S.fromList [minBound .. maxBound]
                    | otherwise = S.empty
        toggleOne :: Direction2D -> S.Set Direction2D -> S.Set Direction2D
        toggleOne x xs | x `S.member` xs = S.delete x xs
                       | otherwise       = x `S.insert` xs


rmWorkarea :: X ()
rmWorkarea = withDisplay $ \dpy -> do
    a <- getAtom "_NET_WORKAREA"
    r <- asks theRoot
    io (deleteProperty dpy r a)

-- | (Initial x pixel, initial y pixel,
--    final x pixel, final y pixel).
newtype RectC = RectC (CLong, CLong, CLong, CLong) deriving (Eq,Show)

-- | Invertible conversion.
r2c :: Rectangle -> RectC
r2c (Rectangle x y w h) = RectC (fi x, fi y, fi x + fi w - 1, fi y + fi h - 1)

-- | Invertible conversion.
c2r :: RectC -> Rectangle
c2r (RectC (x1, y1, x2, y2)) = Rectangle (fi x1) (fi y1) (fi $ x2 - x1 + 1) (fi $ y2 - y1 + 1)


reduce ::
    RectC -> -- root window coordinates
    Struts -> -- struts
    RectC -> -- window coordinates to reduce
    RectC
reduce
    (RectC (min_x, min_y, max_x, max_y))
    (Struts lef rig top bot)
    (RectC (rx0, ry0, rx1, ry1))
    = RectC (rx0', ry0', rx1', ry1')
  where
    -- Is it worth testing whether thickness is 0?
    rx0' = case lef of
        StrutD thic beg end | overlaps (beg, end) (ry0, ry1) ->
            max rx0 (min_x + thic)
        _ -> rx0

    rx1' = case rig of
        StrutD thic beg end | overlaps (beg, end) (ry0, ry1) ->
            min rx1 (max_x - thic)
        _ -> rx1

    ry0' = case top of
        StrutD thic beg end | overlaps (beg, end) (rx0, rx1) ->
            max ry0 (min_y + thic)
        _ -> ry0

    ry1' = case bot of
        StrutD thic beg end | overlaps (beg, end) (rx0, rx1) ->
            min ry1 (max_y - thic)
        _ -> ry1


data Struts = Struts
    !StrutD -- Left
    !StrutD -- Right
    !StrutD -- Top
    !StrutD -- Bottom
  deriving (Eq, Typeable)

noStruts :: Struts
noStruts = Struts noStrut noStrut noStrut noStrut


--                thickness start  end
data StrutD = StrutD !CLong !CLong !CLong
 deriving (Eq, Typeable)

noStrut :: StrutD
noStrut = StrutD 0 minBound maxBound


-- | Do the two ranges overlap?
--
-- Precondition: For every input range @(x, y)@, @x '<=' y@.
--
-- A range @(x, y)@ includes every pixel from @x@ to @y@. E.g. range (0, 0) is one pixel long.
overlaps :: Ord a => (a, a) -> (a, a) -> Bool
ab `overlaps` xy = not (disjoint ab xy)
  where
    disjoint (a, b) (x, y) = b < x  ||  y < a
