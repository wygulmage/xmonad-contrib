
Lenses and traversals that mimic their counterparts in `XMonad.StackSet`, `XMonad.Core`, and `XMonad.Operations` are found in `XMonad.Util.Optics`.

In general, there's a Lens named `_x` at the most precise level and a Lens named `_xs` at the most precise level that still covers the most targets. For example, `_tag` is a Lens from `Workspace`s to workspaceIds, and `_tags` is a Traversal from `StackSet`s to workspaceIds.


So-called 'classy' lenses and traversals are in `XMonad.Util.Optics.Classy` as well as the `XMonad.Util.Optics.Has*` modules. I used an almost completely brute-force approach: For every record field `x` there's a `HasXs` class with an `_xs` traversal, and a `HasX` subclass with an `_x` lens. The class hierarchy matches the data hierarchy.
  For example, `HasTags` is a superclass of `HasTag` and `HasWorkspaces`, and `HasTag` and `HasWorkspaces` are superclasses of `HasWorkspace`.
  This leads to a truly ridiculous number of instances, most of which are defaulted. So there's a ton of boilerplate. This is a case where Template Haskell would really shine, but that's a nonstarter in XMonad.

One exception is `_screens` which is a lens to a nonempty list.
