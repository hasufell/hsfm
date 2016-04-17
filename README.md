HSFM
====

__NOTE: This project is in a highly experimental state! Don't complain if it deletes your whole home directory. You should use a chroot, docker environment or similar for testing.__

A Gtk+:3 filemanager written in Haskell.

Design goals:

- easy to use
- useful library interface to be able to build other user interfaces
- type safety, runtime safety, strictness
- simple add-on interface

Screenshots
-----------

![Image missing](https://cloud.githubusercontent.com/assets/1241845/14584163/6dbef950-0439-11e6-8a6e-2352c048775e.png "hsfm-gtk")

Installation
------------

```
git submodule update --init --recursive
cabal sandbox init
cabal sandbox add-source 3rdparty/hinotify
cabal sandbox add-source 3rdparty/hpath
cabal sandbox add-source 3rdparty/hpath/3rdparty/posix-paths
cabal sandbox add-source 3rdparty/simple-sendfile
cabal install alex happy
export PATH="$(pwd)/.cabal-sandbox/bin:$PATH"
cabal install gtk2hs-buildtools
cabal install
```


Contributing
------------

See [HACKING.md](hacking/HACKING.md).
