Easyspec: Automatic Functional Property Discovery using Signature Inference in Haskell
======================================================================================


# Disclaimer

This is a proof-of-concept of the automation and the signature inference strategies described in [the master thesis 'Signature inference for functional property discovery and test generation'](https://github.com/NorfairKing/thesis).
Because this software was built within the constraints of a master thesis, corners were cut to ensure that the research could be done.
This is by no means production-ready.
There are a lot of wrinkles to be ironed out before this can be used in practice, but you can already start to play with it.
There is more research to be done on this subject, but more importantly more real software engineering.
Please [contact the author](https://cs-syd.eu/contact) if you would like to get involved.

# Installing `easyspec` from source

First, get the source:

``` shell
git clone https://github.com/NorfairKing/easyspec
```

Then, use [`stack`](https://haskellstack.org/) to install `easyspec`:

``` shell
cd easyspec
stack install
```

# Usage

Running `easyspec` without arguments or `easyspec --help` will show you the usage of `easyspec`.

# Dependencies

To discover properties, the internals of the ghc API that `easyspec` uses must have access to `QuickSpec`.
You can either install `quickspec` globally with `cabal install quickspec`, or you can have `stack` arrange everything for you by using `stack exec` to run the commands described below.
For example, instead of running `easyspec discover MyFile.hs`, you would have to run `stack exec easyspec -- discover MyFile.hs`.

# Discovering properties

To discover the properties of a functlon `func` in a file `File.hs` in the directory 'example-dir', you can run `easyspec discover File.hs File.func --base-dir example-dir`.
Easyspec will find all the functions that are in scope at the top-level of a module (including all the imported functions).
Then it performs its magic and uses `quickspec` to discover the properties of the chosen function with respect to all the other functions in scope.

## Example

There are plenty of examples in the `examples` directory, but here is a worked example:

Suppose you have the file `MySort.hs` in your working directory with the following contents:

``` Haskell
{-# LANGUAGE NoImplicitPrelude #-}

module MySort where

import Prelude (Bool(True), otherwise, (&&), Ord((<=)))

mySort :: Ord a => [a] -> [a]
mySort [] = []
mySort (x:xs) = insert (mySort xs)
  where
    insert [] = [x]
    insert (y:ys)
        | x <= y = x : y : ys
        | otherwise = y : insert ys

myIsSorted :: Ord a => [a] -> Bool
myIsSorted [] = True
myIsSorted [_] = True
myIsSorted (x:y:ls) = x <= y && myIsSorted (y : ls)
```

... and suppose you are interested in the properties of the `mySort` function in this file.
This function `mySort` is called the 'focus function'.
Now you can run `stack exec easyspec -- discover MySort.hs mySort` to discover the properties of `mySort` with respect to the functions that are in scope.
The output should look something like the following:

```
$ ls MySort.hs 
MySort.hs
$ stack exec easyspec -- discover MySort.hs MySort.mySort
myIsSorted (mySort xs) = True
mySort (mySort xs) = mySort xs
xs <= mySort xs = myIsSorted xs
mySort xs <= xs = True
```

If you don't specify a focus function, and use the `full-background` strategy, `easyspec` will find the properties of _all_ the functions in scope.
Take care, this may take a lot longer.
The output should look something like the following:

```
$ stack exec easyspec -- discover MySort.hs MySort.mySort --strategy=full-background
otherwise = True
x && x = x
x && True = x
True && x = x
y && x = x && y
y <= y = True
y <= True = True
True <= x = x
x && (x && y) = x && y
(x && y) && z = x && (y && z)
myIsSorted (mySort xs) = True
mySort (mySort xs) = mySort xs
(y && y) <= z = y <= (y && z)
xs <= mySort xs = myIsSorted xs
mySort xs <= xs = True
```

As part of the research, multiple signature inference strategies were developed.
You can try them out as well using the `--strategy` flag.

# Evaluating strategies and experimenting with strategies using `easyspec-evaluate`

`easyspec-evaluate` is a tool that is used to evaluate the different signature inference strategies, generate plots, and eventually hopefully draw helpful conclusions.

The main component is a build system that knows how to make, evaluate and plot data.
You can use it with `stack exec easyspec-evaluate -- build myTarget`.
To build all plots, and transitive dependencies of those plots, use `stack exec easyspec-evaluate -- build analyse`.


# Contributions

Contributions are welcome in many forms (vaguely in the order of time investment):

- An issue suggesting an improvement
- An issue that brings a bug to the attention
- A PR with a file in `examples` that shows functionality that is not shown by other examples.
- A PR with a file in `examples-wishlist` that shows me that certain functionality is missing.
- A PR with a failing test case
- A PR with a bugfix
- A PR with a new signature inference strategy evaluator
- A PR with a new signature inference strategy
- Any work that gets this project closer to being used in production.

Make sure to [install the `zift.hs` script](https://github.com/NorfairKing/zifter) and that any PR passes the continuous integration.
