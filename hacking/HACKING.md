HACKING
=======

Check out the [issue tracker](https://github.com/hasufell/hsfm/issues)
if you don't know yet what you want to hack on.

Coding style
------------

- match the sorroundings
- no overcomplicated pointfree style
- normal indenting 2 whitespaces
- just make things pretty and readable
- use the provided [hsimport.hs](hsimport.hs)

Documentation
-------------

__Everything__ must be documented. :)

Hacking Guide
-------------

The main data structure for the IO related File type is in
[HSFM.FileSystem.FileType](./../src/HSFM/FileSystem/FileType.hs#L93), which
should be seen as a library. This is the entry point where
[directory contents are read](./../src/HSFM/FileSystem/FileType.hs#L465)
and the File type in general [is constructed](./../src/HSFM/FileSystem/FileType.hs#L302).
The File type uses a safe Path type under the hood instead of Strings,
utilizing the [hpath](https://github.com/hasufell/hpath) library.

File operations (like copy, delete etc) are defined at
[HSFM.FileSystem.FileOperation](./../src/HSFM/FileSystem/FileOperations.hs)
which use this File type.

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

