---
title: Installing
---

##Stack

`stack setup`

`stack install`

##Cabal 

Create a sandbox (recommended) with:

`cabal sandbox init`

Install dependencies:

`cabal install --dependencies-only`

Now build the actual project using:

`cabal build`

It will place the jcc binary in `dist/build/jcc/jcc`.
