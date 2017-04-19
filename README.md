Easyspec: Automatic functional property discovery in Haskell
============================================================

# Installation

`stack install` should do the trick.

# Usage

Running `easyspec` without arguments or `easyspec --help` will show you the usage of `easyspec`.


## Discovering properties

To discover properties, ghc must have access to `QuickSpec`.
You can either install quickspec globally with `cabal install quickspec`, or you can have `stack` arrange everything for you by using `stack exec` to run the commands described below.
For example, instead of running `easyspec discover MyFile.hs`, you would have to run `stack exec easyspec discover MyFile.hs`.


To discover the properties of a function `func` in a file `File.hs`, you can run `easyspec discover File.hs func`.
Easyspec will find all the functions that are in scope at the top-level of a module (including all the imported functions).
Then it performs its magic and uses `quickspec` to discover the properties of the chosen function with respect to all the other functions in scope.
