This project is intended as a demonstration of **STM finalizers**.
A finalizer is an arbitrary I/O action that can be attached to an STM transaction using the function
    
    atomicallyWithIO :: STM a -> (a -> IO b) -> IO b

A full specification of STM finalizers, including an operational semantics and a discussion of their implementation in GHC, as well as plenty of other examples of their use, can be found in my [master's thesis](https://github.com/mcschroeder/thesis). There you will also find an in-depth description of this example project.

# Example: A simple social network

The folders `social0`-`social3` contain a modest social networking application, in various stages of completion.
They all contain a Haskell web server exposing a RESTful API (`Server.hs`) and a JavaScript client (`client.html`), but the interesting stuff happens in the site's back-end (`SocialDB.hs`).
There, the network's data is managed using STM.

* The `social0` version uses just STM, meaning the whole database stays purely in-memory.

* In `social1`, we use a lightweight reusable database framework built on top of STM with finalizers. The framework itself can be found in the `tx` folder.  

* The `social2` version demonstrates schema migration: it has some additional features, so its data types differ from the previous version; but thanks to [`SafeCopy`](http://hackage.haskell.org/package/safecopy), we can still open the old database file and continue seamlessly.

* The `social3` version uses [transactional tries](http://hackage.haskell.org/package/ttrie).

# Simulating finalizers

In order for the examples to work, you either need [a patched version of GHC that supports STM finalizers](http://github.com/mcschroeder/ghc) or you can just compile with the `-DSIMULATE_FINALIZERS` flag, e.g.

    cabal repl social1 --ghc-options="-DSIMULATE_FINALIZERS"
