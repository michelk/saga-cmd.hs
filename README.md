# Description

Haskell Library to wrap `saga_cmd`.

Three exectuables are currently shipped

-   **sagaPipe:** A program to chain different saga-calls, where the output of
    one command gets the input of the next.
-   **sagaTopo:** Wrapper to create topographic-maps out of `sgrid`-files
    
    For example
    
        sagaTopo --min 280 --max 360  -o dem.tif dem.sgrd

-   **sagaLut:** Program to create color lookup-tables with min/max values
    eg `sagaLut 260 280 > colors.txt`

This is *Work In Progress*.

## Implemented modules

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />

<col  class="right" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Command</th>
<th scope="col" class="left">(cmdPar,sagaPar,default)</th>
<th scope="col" class="left">sagaLib</th>
<th scope="col" class="right">sagaModule</th>
<th scope="col" class="left">defaultSuffix</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">gridClassToPoly</td>
<td class="left">(all,CLASS<sub>ALL</sub>,0):(id,CLASS<sub>ID</sub>,1):(split,SPLIT,0)</td>
<td class="left">shapes<sub>grid</sub></td>
<td class="right">6</td>
<td class="left">\_polygons.shp</td>
</tr>


<tr>
<td class="left">gridClassifyFlat</td>
<td class="left">(method,METHOD,2):(table,RETAB,reclassify.txt)</td>
<td class="left">grid<sub>tools</sub></td>
<td class="right">15</td>
<td class="left">\_reclassified.sgrd</td>
</tr>


<tr>
<td class="left">gridContour</td>
<td class="left">(d,ZSTEP,1):(max,ZMAX,10000):(min,ZMIN,0)</td>
<td class="left">shapes<sub>grid</sub></td>
<td class="right">5</td>
<td class="left">\_contour.sgrd</td>
</tr>


<tr>
<td class="left">gridFillGaps</td>
<td class="left">(grdFlT,TARGET,1)</td>
<td class="left">grid<sub>spline</sub></td>
<td class="right">5</td>
<td class="left">\_filled.sgrd</td>
</tr>


<tr>
<td class="left">gridHillshade</td>
<td class="left">NA</td>
<td class="left">ta<sub>lighting</sub></td>
<td class="right">0</td>
<td class="left">\_hillshade.sgrd</td>
</tr>


<tr>
<td class="left">gridPolyClip</td>
<td class="left">(poly,POLYGONS,)</td>
<td class="left">shapes<sub>grid</sub></td>
<td class="right">7</td>
<td class="left">\_polyClip.sgrd</td>
</tr>


<tr>
<td class="left">gridSlope</td>
<td class="left">(aspect,ASPECT,aspect)</td>
<td class="left">ta<sub>morphometry</sub></td>
<td class="right">0</td>
<td class="left">\_slope.sgrd</td>
</tr>


<tr>
<td class="left">gridTifGdal</td>
<td class="left">NA</td>
<td class="left">io<sub>gdal</sub></td>
<td class="right">2</td>
<td class="left">.tif</td>
</tr>


<tr>
<td class="left">gridTifHillshade</td>
<td class="left">(col,COL<sub>PALETTE</sub>,2):(colRev,COL<sub>REVERT</sub>,)</td>
<td class="left">io<sub>grid</sub><sub>image</sub></td>
<td class="right">0</td>
<td class="left">.tif</td>
</tr>


<tr>
<td class="left">gridXyz</td>
<td class="left">NA</td>
<td class="left">io<sub>grid</sub></td>
<td class="right">5</td>
<td class="left">.xyz</td>
</tr>


<tr>
<td class="left">lasToPtCld</td>
<td class="left">NA</td>
<td class="left">io<sub>shapes</sub><sub>las</sub></td>
<td class="right">1</td>
<td class="left">.spc</td>
</tr>


<tr>
<td class="left">polyDissolve</td>
<td class="left">(f1,FIELD<sub>1</sub>,1):(f2,FIELD<sub>1</sub>,-1):(f3,FIELD<sub>1</sub>,-1):(method,DISSOLVE,0)</td>
<td class="left">shapes<sub>polygons</sub></td>
<td class="right">5</td>
<td class="left">\_disollved.shp</td>
</tr>


<tr>
<td class="left">ptCldToGrid</td>
<td class="left">(cs,CELLSIZE,1)</td>
<td class="left">pointcloud<sub>tools</sub></td>
<td class="right">4</td>
<td class="left">.sgrd</td>
</tr>


<tr>
<td class="left">xyzGridToGrid</td>
<td class="left">(cs,CELLSIZE,1):(sep,SEPARATOR,space)</td>
<td class="left">io<sub>grid</sub></td>
<td class="right">6</td>
<td class="left">.sgrd</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="right">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>
</table>

# Installation

## Saga And haskell-platform

### Linux

    sudo aptitude install haskell-platform
    sudo aptitude install saga

### Windows with Cygwin

-   Get the [haskell-platform](http://www.haskell.org/platform/)
-   Install Saga with [OSGeo4W](http://trac.osgeo.org/osgeo4w/)
-   Put `saga_cmd` in the search path; add the line following (the actual
    filepath could differ) line to your `$HOME/.bashrc`
    
        export PATH="$PATH:/cygdrive/c/Program Files (x86)/SAGA-GIS

## saga-cmd-wrapper library

    git clone https://www.github.com/michelk/bindings-saga-cmd.hs.git
    cd bindings-saga-cmd.hs
    cabal update && cabal install

The exectuables are usually installed into `$HOME/.cabal/bin`; to
use them you have to put them into your search path. Put this into
your `$HOME/.bashrc`:

    export PATH="$PATH:$HOME/.cabal/bin"

# `sagaPipe`

## Usage

    $HOME/.cabal/bin/sagaPipe --help

`sagaPipe` lets you chain `saga_cmd` commands. The program could be
used in two ways. Common for both modes are specification of
parameters to use during conversion and the input-file.

-   **Source-Target:** Specify a source-format and target-format
-   **Processing-pathway:** Specify pathway/route to go exlicitly

The pathways are visualized below

![img](doc/figures/chains.png)

## Example

### Source-Target

    sagaPipe --from xyz-grid --to grid-filled-hillshade-tif --parameters sep=tabulator:cs=0.5 -o dem_hillshade.tif dem.xyz

If `-o` hadn't been specified, in the directory of the input-file, the follwing grids would have been created:

-   `dem_grid.sgrd`
-   `dem_grid-filled.sgrd`
-   `dem_grid-filled_hillshade.sgrd`

### Processing-pathway

The equivalent exlicit version to above is

    sagaPipe --chain xyzGridToGrid:gridFillGaps:gridHillshade --parameters sep=tabulator:cs=0.5 dem.xyz

Or for example If you would like to create a hillshade with some
buildings removed; invert the buildings-shapefile and do the
following

    sagaPipe --chain xyzGridToGrid:gridFillGaps:gridHillshade:gridPolyClip:gridTifHillshade  \
             -p poly=BuildingsInv.shp:cs=0.5:sep=tabulator dem.xyz

## `sagaTopo`

Wrapper-program to create topographic maps from `sgrd`-files

# Development

In order to extend functionality, the follwing steps are necessary
-   Find the wanted library and module
    -   Eg with `./doc/search`
-   Edit `src/Math/Geometry/Saga/Data.hs`
    -   create a SagaCmd (copy & paste an existing one; pretty straigt forward)
    -   create a pathway-chain
-   Export `README.org`

## Find the module

    cd saga-cmd-wrapper/do
    ./search <keyword> 

## New SagaCmd

Edit `src/Math/Geometry/Saga/Data.hs`
-   Add Module
-   Add pathway node

## Documentation

-   Edit `README.org`
-   Export it to markdown (`M-x org-md-export-to-markdown`)

# ToDo

-   extend library-commands
-   merge `sagaTopo` into `sagaPipe`
-   cmd-line-option which color-palette to use (`sagaLut`)
