HACKING
=======

Coding style
------------

- match the sorroundings
- no overcomplicated pointfree style
- normal indenting 2 whitespaces
- just make things pretty and readable

Documentation
-------------

__Everything__ must be documented. :)

Hacking Guide
-------------

The main data structure is in [DirTree.hs](src/Data/DirTree.hs), which
should be seen as a library. This is then mapped into the Gtk+ GUI at
[Gtk.hs](src/GUI/Gtk.hs) and [Utils.hs](src/GUI/Gtk/Utils.hs).

File operations (like copy, delete etc) are defined at
[File.hs](src/IO/File.hs).

Note that the main data structures are still a bit in flux. Join
[the discussion](https://github.com/hasufell/hsfm/issues/12) on how to
improve them.
