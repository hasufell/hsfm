HSFM
====

[![Join the chat at https://gitter.im/hasufell/hsfm](https://badges.gitter.im/hasufell/hsfm.svg)](https://gitter.im/hasufell/hsfm?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://api.travis-ci.org/hasufell/hsfm.png?branch=master)](http://travis-ci.org/hasufell/hsfm)

A Gtk+:3 filemanager written in Haskell.

Design goals:

- easy to use
- useful library interface to be able to build other user interfaces
- type safety, runtime safety, strictness
- simple add-on interface

Screenshots
-----------

![hsfm](https://cloud.githubusercontent.com/assets/1241845/15807148/fc6238d2-2b55-11e6-9011-16d41c6d9d1a.png "hsfm-gtk")

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
