module Math.Geometry.Saga.Data where
import Math.Geometry.Saga.Types
import Math.Geometry.Saga.Utils 
import qualified Data.Map as M

cXyzGridToGrid, cGridFillGaps, cGridHillShade :: ChainSagaIoCmd
cGridContour, cLasToPtCld, cPtCldToGrid :: ChainSagaIoCmd

cXyzGridToGrid = (xyzGridToGrid, ".sgrd")
cGridFillGaps  = (gridFillGaps,  "_filled.sgrd")
cGridHillShade = (gridHillShade, "_hillshade.sgrd")
cGridContour   = (gridContour,   "_contour.sgrd")
cLasToPtCld    = (lasToPtCld,    ".pcl")
cPtCldToGrid   = (ptCldToGrid,   ".sgrd")

xyzGridToGrid, gridFillGaps, gridHillShade, gridContour, lasToPtCld, gridTopo :: SagaIoCmd
xyzGridToGrid =
  SagaCmd "libio_grid" "6" ("FILENAME","GRID")
  (M.fromList [
       ("cs",  ("CELLSIZE"  , "1"))
      ,("sep", ("SEPARATOR" , "space"))
      ])
  Nothing Nothing

lasToPtCld =
  SagaCmd "libio_shapes_las" "1" ("POINTS","FILE")
  (M.fromList []) Nothing Nothing

ptCldToGrid =
  SagaCmd "libpointcloud_tools" "4" ("POINTS","GRID")
  (M.fromList []) Nothing Nothing

gridFillGaps =
  SagaCmd "libgrid_spline" "5" ("GRIDPOINTS","GRID_GRID")
  (M.fromList [("grdFlT", ("TARGET", "1"))])
  (Just copyGrid)
  Nothing

gridHillShade =
  SagaCmd "libta_lighting" "0" ("ELEVATION","SHADE")
  (M.fromList []) Nothing Nothing

gridContour =
  SagaCmd "libshapes_grid" "5" ("INPUT","CONTOUR")
  (M.fromList [
       ("min" , ("ZMIN"  , "0"))
      ,("max" , ("ZMAX" , "10000"))
      ,("d",    ("ZSTEP" , "1"))
      ]) Nothing Nothing

gridTopo =
  SagaCmd "libio_grid_image " "0" ("GRID", "FILE")
  (M.fromList [
      ("min",     ("", undefined))
     ,("max",     ("", undefined))
      ]) Nothing Nothing

dispatchGridTopo :: Float -> Float -> SagaIoCmd
dispatchGridTopo min max = undefined


-- | Processing chains
sChainDB :: ChainDB
sChainDB = M.fromList [
    (("las"        , "grid")        , [cLasToPtCld,cPtCldToGrid])
   ,(("las"        , "grid-filled") , [cLasToPtCld,cPtCldToGrid, cGridFillGaps])
   ,(("las"        , "hillshade")   , [cLasToPtCld,cPtCldToGrid, cGridFillGaps, cGridHillShade])
   ,(("las"        , "contour")     , [cLasToPtCld,cPtCldToGrid, cGridFillGaps , cGridContour])
   ,(("xyz-grid"   , "grid")        , [cXyzGridToGrid])
   ,(("xyz-grid"   , "grid-filled") , [cXyzGridToGrid, cGridFillGaps])
   ,(("xyz-grid"   , "hillshade")   , [cXyzGridToGrid, cGridFillGaps , cGridHillShade])
   ,(("xyz-grid"   , "contour")     , [cXyzGridToGrid, cGridFillGaps , cGridContour])
   ,(("grid"       , "hillshade")   , [cGridFillGaps, cGridHillShade])
   ,(("grid"       , "grid-filled") , [cGridFillGaps])
   ,(("grid"       , "hillshade")   , [cGridFillGaps, cGridHillShade])
   ,(("grid"       , "contour")     , [cGridFillGaps, cGridContour])
   ,(("grid-filled", "hillshade")   , [cGridHillShade])
   ,(("grid-filled", "contour")     , [cGridContour])
   ]
