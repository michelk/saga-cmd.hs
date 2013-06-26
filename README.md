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

    ghci> :m Math.Geometry.Saga.Cmd
    ghci> xyzGridToGrid 1 "space" "dem.xyz"

### demConv

Or you could use `demConv`. Eg

    demnConv --from xyz --to hillshade --parameters xyzCellSize=1:xyzSep=tab

#### Possible from-to-combinatinations


              from          to
              grid         tif
        grid-filled   hillshade
              grid   hillshade
          xyz-grid         tif
              grid     contour
          xyz-grid  grid-filled
        grid-filled         tif
        grid-filled     contour
          xyz-grid   hillshade
          xyz-grid        grid
          xyz-grid     contour

##### Default parameters:

        xyzCellSize           1
        contourMax       10000
        contourStep           1
         tinMethod  Opposite Neighbours
            xyzSep       space
        contourMin           0

Development
-----------

In order to extend functionality

* find library and module
* write wrapper-function
* add default parameters and conversion-function in `demConv`

### Finding the library and module to add
When extending the library, the first thing to do is finding the library and
module you want to add. 

    cd doc
    ./search <keyword>

Eg looking for xyz-import/export

    ./search xyz 
    ### libio_shapes: 2 :Export Shapes to XYZ
    ### libio_shapes: 3 :Import Shapes from XYZ
    ### libio_grid: 5   :Export Grid to XYZ
    ### libio_grid: 6   :Import Grid from XYZ

### Add wrapper-function in `Math.Geometry.Saga.Cmd`

### Adjust parameters in `demConv`
