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

![hsfm](https://cloud.githubusercontent.com/assets/1241845/14768900/06efd43c-0a4d-11e6-939e-6b067bdb47ce.png "hsfm-gtk")

Installation
------------

```
cabal sandbox init
cabal install alex happy
export PATH="$(pwd)/.cabal-sandbox/bin:$PATH"
cabal install gtk2hs-buildtools
cabal install
```


Contributing
------------

See [HACKING.md](hacking/HACKING.md).
