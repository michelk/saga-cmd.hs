module Gis.Saga.Data where
import Gis.Saga.Types
import Gis.Saga.Utils
import System.Directory (removeFile)
import qualified Data.Map as M

-- | Implemented command for piping
sIoDB :: SagaIoCmdDB
sIoDB = M.fromList [
  ("xyzGridToGrid", (
      SagaCmd "io_grid" "6" ("FILENAME","GRID") -- library, module, input-,output-parameters
              (M.fromList [     -- ^ parameters: sagaPipe-par, saga_cmd-par, default
                   ("cs",  ("CELLSIZE"  , "1"))
                  ,("sep", ("SEPARATOR" , "space"))
                  ])
              Nothing Nothing   -- pre-, post-processing
      , ".sgrd"                 -- ^ output-file extension
      )
  )
  ,("lasToPtCld", (
       SagaCmd "io_shapes_las" "1" ("FILE", "POINTS")
       (M.fromList []) Nothing Nothing , ".spc"))
  ,("ptCldToGrid", (
       SagaCmd "pointcloud_tools" "4" ("POINTS","GRID")
       (M.fromList [
                   ("cs",  ("CELLSIZE"  , "1"))
                   ])
       Nothing Nothing ,".sgrd"))
  ,("gridFillGaps", (
       SagaCmd "grid_spline" "5" ("GRIDPOINTS","GRID_GRID")
       (M.fromList [("grdFlT", ("TARGET", "1"))])
       (Just copyGrid) Nothing, "_filled.sgrd"))
  ,("gridHillshade", (
       SagaCmd "ta_lighting" "0" ("ELEVATION","SHADE")
       (M.fromList []) Nothing Nothing , "_hillshade.sgrd"))
  ,("gridContour", (
       SagaCmd "shapes_grid" "5" ("INPUT","CONTOUR")
       (M.fromList [
            ("min" , ("ZMIN"  , "0"))
           ,("max" , ("ZMAX" , "10000"))
           ,("d",    ("ZSTEP" , "1"))
           ]) Nothing Nothing, "_contour.sgrd"))
  ,("gridPolyClip", (
       SagaCmd "shapes_grid" "7" ("INPUT","OUTPUT")
       (M.fromList [
           ("poly" , ("POLYGONS"  , ""))
           ]) Nothing Nothing, "_polyClip.sgrd"))
  ,("gridTifHillshade", (
       SagaCmd "io_grid_image" "0" ("GRID","FILE")
       (M.fromList [
           ("pal" ,  ("COL_PALETTE", "2"))
          ,("colRev",("COL_REVERT" , ""))
          ,("min",("STRETCH_MIN" , "0"))
          ,("max",("STRETCH_MAX" , "1.571"))
          --,("stddev",("STDDEV"   , "2"))
          ,("col",("COLOURING"   , "2"))
           ]) Nothing Nothing, ".tif"))
  ,("gridTifTerrain", (
       SagaCmd "io_grid_image" "0" ("GRID","FILE")
       (M.fromList [
           ("pal" ,  ("COL_PALETTE", "23"))
          --,("colRev",("COL_REVERT" , ""))
          ,("min",("STRETCH_MIN" , "0"))
          ,("max",("STRETCH_MAX" , "100"))
          --,("stddev",("STDDEV"   , "2"))
          ,("col",("COLOURING"   , "2"))
           ]) Nothing Nothing, ".tif"))
  ,("gridTifGdal", (
       SagaCmd "io_gdal" "2" ("GRIDS","FILE")
       (M.fromList [
           ]) Nothing Nothing, ".tif"))
  ,("gdalGrid", (
       SagaCmd "io_gdal" "0" ("FILES", "GRIDS")
       (M.fromList [
           ]) Nothing Nothing, ".sgrd"))
  ,("gridXyz", (
         SagaCmd "io_grid" "5" ("GRIDS","FILENAME")
         (M.fromList [
             ]) Nothing Nothing, ".xyz"))
  ,("polyDissolve", (
         SagaCmd "shapes_polygons" "5" ("POLYGONS","DISSOLVED")
         (M.fromList [
           ("method" ,("DISSOLVE" , "0")) -- 0: polygons with same attribute value
                                         -- 1: all polygons
                                         -- 2: polygons with same attribute value (keep inner boundaries)
                                         -- 3: all polygons (keep inner boundaries)
          ,("f1",("FIELD_1"  , "1"))
          ,("f2",("FIELD_1"  , "-1"))
          ,("f3",("FIELD_1"  , "-1"))
         ])
         Nothing Nothing, "_disollved.shp"))
  ,("gridSlope", (
         SagaCmd "ta_morphometry" "0" ("ELEVATION","SLOPE")
         (M.fromList [
            ("aspect" ,("ASPECT" , "aspect"))
         ])
         Nothing Nothing, "_slope.sgrd"))
  ,("gridClassifyFlat", (
         SagaCmd "grid_tools" "15" ("INPUT","RESULT")
         (M.fromList [
            ("method" ,("METHOD" , "2")) -- 1: range
                                         -- 2: simple table
           ,("table" ,("RETAB" , "reclassify.txt"))
         ])
         (Just (\_ _ -> writeReclassifyTableFlatSlope "reclassify.txt"))
         (Just (\_ _ -> removeFile "reclassify.txt"))
                , "_reclassified.sgrd"))
  ,("gridClassToPoly", (
         SagaCmd "shapes_grid" "6" ("GRID","POLYGONS")
         (M.fromList [
           ("id",    ("CLASS_ID" , "1")) -- class identifier
          ,("all",   ("CLASS_ALL" , "0")) -- 0: one single class specified by class identifier
                                          -- 1: all classes
          ,("split",  ("SPLIT" , "0")) -- 0: one single (multi-)polygon object
                                       -- 1: each island as separated polygon
         ])
         Nothing Nothing, "_polygons.shp"))
  ]


-- | Pathsway nodes with input and output commands
sNodes :: NodeMap
sNodes =
  M.fromList [
    ("las", ([], ["lasToPtCld"]))
   ,("gdal", ([], ["gdalGrid"]))
   ,("ptc", (["lasToPtCld"], ["ptCldToGrid"]))
   ,("grid", (["ptCldToGrid", "xyzGridToGrid", "gdalGrid"],
      ["gridFillGaps", "gridTifGdal", "gridPolyClip"
      ,"gridSlope", "gridClassToPoly", "gridClassifyFlat"]))
   ,("xyz-grid", ([], ["xyzGridToGrid"]))
   ,("grid-filled", (["gridFillGaps"],
      ["gridHillshade", "gridXyz", "gridContour"]))
   ,("grid-filled-hillshade", (["gridHillshade"], ["gridTifHillshade"]))
   ,("grid-filled-hillshade-tif", (["gridTifHillshade"], []))
   ,("grid-hillshade", (["gridHillshade"], ["gridTifHillshade"]))
   ,("grid-hillshade-tif", (["gridTifHillshade"], []))
   ,("grid-filled-xyz", (["gridXyz"], []))
   ,("grid-filled-contour", (["gridContour"], []))
   ,("grid-polygonClip", (["gridPolyClip"], []))
  ]
