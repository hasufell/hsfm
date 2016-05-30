HSFM
====

[![Build Status](https://api.travis-ci.org/hasufell/hsfm.png?branch=master)](http://travis-ci.org/hasufell/hsfm)

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
