# HACKING

Check out the [issue tracker](https://github.com/hasufell/hsfm/issues)
if you don't know yet what you want to hack on.

## Coding style

- match the sorroundings
- no overcomplicated pointfree style
- normal indenting 2 whitespaces
- just make things pretty and readable
- you can use the provided [hsimport.hs](hsimport.hs)

## Documentation

__Everything__ must be documented. :)
Don't assume people know what you mean. Type signatures are not sufficient
documentation.

## Hacking Overview

Only a GTK GUI is currently implemented, the entry point being
[HSFM.GUI.Gtk](./../src/HSFM/GUI/Gtk.hs). From there it flows down
to creating a [MyGUI object](./../src/HSFM/GUI/Gtk/Data.hs#L51) in
[HSFM.GUI.Gtk.MyGUI](./../src/HSFM/GUI/Gtk/MyGUI.hs), which is sort of
a global object for the whole window. Inside this object are
theoretically multiple [MyView objects](./../src/HSFM/GUI/Gtk/Data.hs#L101)
allowed which represent the actual view on the filesystem and related
widgets, which are constructed in
[HSFM.GUI.Gtk.MyView](./../src/HSFM/GUI/Gtk/MyView.hs). Both MyGUI and MyView
are more or less accessible throughout the whole GTK callstack, expclicitly
passed as parameters.

For adding new GTK widgets with functionality you mostly have to touch the
following files:
* [builder.xml](./../data/Gtk/builder.xml): this defines the main GUI widgets which are static, use the [glade editor](http://glade.gnome.org) to add stuff
* [HSFM.GUI.Gtk.Data](./../src/HSFM/GUI/Gtk/Data.hs): add the widget to e.g. the MyGUI type so we can access it throughout the GTK call stack
* [HSFM.GUI.Gtk.MyGUI](./../src/HSFM/GUI/Gtk/MyGUI.hs): add initializers for the GUI buttons to be fetched from the GTK builder.xml file
* [HSFM.GUI.Gtk.Callbacks](./../src/HSFM/GUI/Gtk/Callbacks.hs): define the callbacks and the actual functionality here

## Concepts

### Path safety

Paths are usually represented in haskell libraries as `type FilePath = String`.
This is bad, because of a number of reasons:
* encoding issues, since the low-level representation of filepaths is in fact an array of C chars
* weak typing... we could pass arbitrary invalid/malicious filepaths or other random strings
* no information about any property at type level (e.g. is it an absolute path?)
* no filepath constructors that do sanity checks and proper parsing
* no guarantee whether the filepath is normalised or not or even valid

Because of that, the solution is:
* use `ByteString` under the hood
* wrap it inside `Path t` where `t` can be either `Abs` (for absolute), `Rel` (for relative) or `Fn` (for filename)
* construct filepaths via smart constructors only that reject certain paths (like `.` or `..`) and normalise the path

This leads to the following benefits:
* we have guarantees about whether a path is absolute or not, which is important for runtime safety in general, predictable behavior and thread safety
* we don't mess with the filepath representation we get from low-level posix functions, so encoding issues are pretty much out
* we can reason about filepaths and rely on them to be valid (don't confuse that with "they exist")
* filepath functions like `(</>)` are now predictable and safe in contrast to the version from the `filepath` package

The only problem with this approach is that most libraries are still String
based. Some provide dedicated `Foo.ByteString` modules though, but it
might be necessary to fork libraries.
We also need to keep track of the [Abstract FilePath proposal](https://ghc.haskell.org/trac/ghc/wiki/Proposal/AbstractFilePath).

Almost all paths in HSFM are only allowed to be absolute (`Path Abs`), unless
they are filenames (`Path Fn`) and processed for GUI purposes. This is as
already mentioned for the purpose of runtime safety, predictability and
thread safety.

### File IO safety

This is a pretty difficult problem. One thing to ensure safety on IO level
is simply the strong haskell type system, since we push everything
into our `File a` type and can then pattern match easily against the different
types of files.

The only problem with this approach is that we are examining a file at point
`a` in time, safe the information and then use that information further down
the call stack at point `b` in time, when the file information in memory
could already be out of date. There are two approaches to make this less
sucky:
* use the hinotify library on GUI level to refresh the view (and the File representation in memory) whenever the contents of a directory changes
* when we stuff something into the copy buffer, it is not saved as type `File a`, but as `Path Abs`... when the operation is finalized via `runFileOp`, then the file at the given path is read and the copy/move/whatnot function carried out immediately

This means we should only interact with the `HSFM.FileSystem.FileOperation`
module via the operation data types `FileOperation`, `Copy` and `Move` and
the `runFileOp` function. This doesn't completely solve the problem, but for
the rest we have to trust the posix functions to throw the proper exceptions.

In addition, we don't use the `directory` package, which is dangerous
and broken. Instead, we implement our own low-level wrappers around
the posix functions, so we have proper control over the internals
and know the possible exceptions.

### Exception handling

Exceptions are good. We don't want to wrap everything in Maybe/Either types
unless we want to handle failure immediately. Otherwise we need to make
sure that at least at some point IOExceptions are caught and visualized
to the user. This is often done via e.g. `withErrorDialog` which catches
`IOException` and `FmIOException`.

It's also important to clean up stuff like filedescriptors via
functions like `bracket` directly in our low-level code in case
something goes wrong.

