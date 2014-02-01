Life..
------
Its an implementation of Conway's life game in haskell.

Requirements:

- ghc
- cabal (at least 1.18 to use the sandboxes).

To initialize the and compile project:

	$ cabal sandbox init
	$ cabal configure --enable-tests
	$ cabal install --only-dependencies
	$ cabal build

To run tests:

	$ cabal build && cabal test
