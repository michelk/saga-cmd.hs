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
              (M.fromList [     --  parameters: sagaPipe-par, saga_cmd-par, default
                   ("cs",  ("CELLSIZE"  , "1"))
                  ,("sep", ("SEPARATOR" , "1")) -- 1: space, 2: ',', 3: ';' 4: tabulator
                  ])
              Nothing Nothing   -- pre-, post-processing
      , ".sgrd"                 --  output-file extension
      )
  )
  ,("lasToPtCld", (
       SagaCmd "io_shapes_las" "1" ("FILE", "POINTS")
       M.empty Nothing Nothing , ".sg-pts"))
  ,("xyzToPtCld", (
       SagaCmd "io_shapes" "16" ("FILE", "POINTS")
       (M.fromList [
                    ("sep", ("SEPARATOR" , "1")) -- 0: tab, 1: space, 2: comma
                   ]) Nothing Nothing , ".sg-pts"))
  ,("ptCldToGrid", (
       SagaCmd "pointcloud_tools" "4" ("POINTS","GRID")
       (M.fromList [
           ("cs",  ("CELLSIZE"  , "1"))
          ,("aggr",  ("AGGREGATION"  , "2"))
           ])
       Nothing Nothing ,".sgrd"))
  ,("gridFillGapsSpline", (
    SagaCmd "grid_spline" "5" ("GRID","TARGET_OUT_GRID")
    (M.fromList [
                 ("grdFlT",    ("TARGET_DEFINITION", "1"))
                --,("grdFlTtmpl", ("TARGET_TEMPLATE", out))  -- currently broken
                ])
    (Just copyGrid) Nothing, "_filled.sgrd"))
  ,("gridFillGaps", (
    SagaCmd "grid_tools" "25" ("GRID","CLOSED")
    M.empty
    Nothing Nothing, "_filled.sgrd"))
  ,("gridHillshade", (
       SagaCmd "ta_lighting" "0" ("ELEVATION","SHADE")
       M.empty Nothing Nothing , "_hillshade.sgrd"))
  ,("gridContour", (
       SagaCmd "shapes_grid" "5" ("GRID","CONTOUR")
       (M.fromList [
            ("min" , ("ZMIN"  , "0"))
           ,("max" , ("ZMAX" , "10000"))
           ,("d",    ("ZSTEP" , "1"))
           ]) Nothing Nothing, "_contour.shp"))
  ,("gridPolyClip", (
       SagaCmd "shapes_grid" "7" ("INPUT","OUTPUT")
       (M.fromList [
           ("poly" , ("POLYGONS"  , "1"))
           ]) Nothing Nothing, "_polyClip.sgrd"))
  ,("gridTifHillshade", (
       SagaCmd "io_grid_image" "0" ("GRID","FILE")
       (M.fromList [
           ("pal" ,  ("COL_PALETTE", "2"))
          ,("colRev",("COL_REVERT" , "1"))
          ,("min",("STRETCH_MIN" , "0"))
          ,("max",("STRETCH_MAX" , "1.571"))
          --,("stddev",("STDDEV"   , "2"))
          --,("col",("COLOURING"   , "2"))
           ]) Nothing Nothing, ".tif"))
  ,("gridTifTerrain", (
       SagaCmd "io_grid_image" "0" ("GRID","FILE")
       (M.fromList [
           ("pal" ,  ("COL_PALETTE", "23"))
          --,("colRev",("COL_REVERT" , "1"))
          ,("min",("STRETCH_MIN" , "0"))
          ,("max",("STRETCH_MAX" , "100"))
          --,("stddev",("STDDEV"   , "2"))
          ,("col",("COLOURING"   , "2"))
           ]) Nothing Nothing, ".tif"))
  ,("gridTifGdal", (
       SagaCmd "io_gdal" "2" ("GRIDS","FILE")
       M.empty Nothing Nothing, ".tif"))
  ,("gridEsriAsc", (
       SagaCmd "io_grid" "0" ("GRID","FILE")
       M.empty Nothing Nothing, ".asc"))
  ,("ascGrd", (
       SagaCmd "io_grid" "1" ("FILE","GRID")
       M.empty Nothing Nothing, ".sgrd"))
  ,("gdalGrid", (
       SagaCmd "io_gdal" "0" ("FILES", "GRIDS")
       M.empty Nothing Nothing, ".sgrd"))
  ,("gridXyz", (
         SagaCmd "io_grid" "5" ("GRIDS","FILENAME")
         (M.fromList [
           ("header",("HEADER"  , "0"))
          ,("na",("NODATA"  , "0"))
             ]) Nothing Nothing, ".xyz"))
  ,("polyDissolve", (
         SagaCmd "shapes_polygons" "5" ("POLYGONS","DISSOLVED")
         (M.fromList [
           ("f1",("FIELD_1"  , "-1"))
          ,("f2",("FIELD_2"  , "-1"))
          ,("f3",("FIELD_3"  , "-1"))
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
            ("method" ,("METHOD" , "0")) -- 1: range
                                         -- 2: simple table
                                         -- 0: single
           ,("table" ,("RETAB" , "reclassify.txt")) -- method = 2
           ,("old" ,("OLD" , "0.0"))  -- method = 0
           ,("new" ,("NEW" , "1.0"))  -- method = 0
           ,("nodata" ,("RESULT_NODATA_VALUE" , "-99999.0"))
         ])
         (Just (\_ _ -> writeReclassifyTableFlatSlope "reclassify.txt"))
         (Just (\_ _ -> removeFile "reclassify.txt"))
                , "_reclassified.sgrd"))
  ,("gridClassToPoly", (
         SagaCmd "shapes_grid" "6" ("GRID","POLYGONS")
         (M.fromList [
           ("id",    ("CLASS_ID" , "1")) -- class identifier
          ,("all",   ("CLASS_ALL", "0")) -- 0: one single class specified by class identifier
                                          -- 1: all classes
          ,("split",  ("SPLIT" , "0")) -- 0: one single (multi-)polygon object
                                       -- 1: each island as separated polygon
         ])
         Nothing Nothing, "_polygons.shp"))
  ]


-- | Pathsway nodes with input and output commands
sNodes :: NodeMap
sNodes =
  M.fromList
    [("las",([],["lasToPtCld"]))
    ,("gdal",([],["gdalGrid"]))
    ,("ptc",(["lasToPtCld"],["ptCldToGrid"]))
    ,("grid"
     ,(["ptCldToGrid","xyzGridToGrid","gdalGrid","ascGrd"]
      ,["gridFillGapsSpline"
       ,"gridFillGaps"
       ,"gridTifGdal"
       ,"gridPolyClip"
       ,"gridSlope"
       ,"gridClassToPoly"
       ,"gridClassifyFlat"
       ,"gridEsriAsc"
       ,"gridTifTerrain"
       ,"gridXyz"
       ,"gridHillshade"]))
    ,("xyz-grid",([],["xyzGridToGrid","xyzToPtCld"]))
    ,("grid-filled",(["gridFillGaps", "gridFillGapsSpline"],["gridHillshade","gridXyz","gridContour"]))
    ,("grid-filled-hillshade",(["gridHillshade"],["gridTifHillshade"]))
    ,("grid-filled-hillshade-tif",(["gridTifHillshade"],[]))
    ,("grid-hillshade",(["gridHillshade"],["gridTifHillshade"]))
    ,("grid-hillshade-tif",(["gridTifHillshade"],[]))
    ,("grid-filled-xyz",(["gridXyz"],[]))
    ,("grid-filled-contour",(["gridContour"],[]))
    ,("grid-polygonClip",(["gridPolyClip"],[]))
    ,("grid-terrain-tif",(["gridTifTerrain"],[]))
    ,("grid-esri-asc",(["gridEsriAsc"],[]))
    ,("esri-asc",([],["ascGrd"]))]
