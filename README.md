Haskell Library to wrap `saga_cmd`.

*Work In Progress*

Prerequisites
------------

### Install saga

    sudo aptitude install saga

### Install library

    git clone https://www.github.com/michelk/saga-cmd-wrapper.hs.git
    cd saga-cmd-wrapper.hs
    cabal update
    cabal install

Usage
-----

### Haskell Library
You could either use the haskell-library directly.  Eg.

    ghci> :m System.Saga.Cmd
    ghci> xyzGridToGrid 1 "space" "dem.xyz"

### demConv

Or you could use `demConv`. Eg

    demnConv --from xyz --to hillshade --parameters xyzCellSize=1:xyzSep=tab


