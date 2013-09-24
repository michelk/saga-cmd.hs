module Math.Geometry.Saga.Data where
import Math.Geometry.Saga.Types
import Math.Geometry.Saga.Utils
import System.Directory (removeFile)
import qualified Data.Map as M

-- | Implemented command for piping
sIoDB :: SagaIoCmdDB
sIoDB = M.fromList [
  ("xyzGridToGrid", (
      SagaCmd "libio_grid" "6" ("FILENAME","GRID")
              (M.fromList [
                   ("cs",  ("CELLSIZE"  , "1"))
                  ,("sep", ("SEPARATOR" , "space"))
                  ])
              Nothing Nothing
      , ".sgrd"                 -- ^ output-file extension
      )
  )
  ,("lasToPtCld", (
       SagaCmd "libio_shapes_las" "1" ("POINTS","FILE")
       (M.fromList []) Nothing Nothing , ".pcl"))
  ,("ptCldToGrid", (
       SagaCmd "libpointcloud_tools" "4" ("POINTS","GRID")
       (M.fromList []) Nothing Nothing ,".sgrd"))
  ,("gridFillGaps", (
       SagaCmd "libgrid_spline" "5" ("GRIDPOINTS","GRID_GRID")
       (M.fromList [("grdFlT", ("TARGET", "1"))])
       (Just copyGrid) Nothing, "_filled.sgrd"))
  ,("gridHillshade", (
       SagaCmd "libta_lighting" "0" ("ELEVATION","SHADE")
       (M.fromList []) Nothing Nothing , "_hillshade.sgrd"))
  ,("gridContour", (
       SagaCmd "libshapes_grid" "5" ("INPUT","CONTOUR")
       (M.fromList [
            ("min" , ("ZMIN"  , "0"))
           ,("max" , ("ZMAX" , "10000"))
           ,("d",    ("ZSTEP" , "1"))
           ]) Nothing Nothing, "_contour.sgrd"))
  ,("gridPolyClip", (
       SagaCmd "libshapes_grid" "7" ("INPUT","OUTPUT")
       (M.fromList [
           ("poly" , ("POLYGONS"  , ""))
           ]) Nothing Nothing, "_polyClip.sgrd"))
  ,("gridTifHillshade", (
       SagaCmd "libio_grid_image" "0" ("GRID","FILE")
       (M.fromList [
           ("col" ,  ("COL_PALETTE" , "2"))
          ,("colRev",("COL_REVERT"  , ""))
           ]) Nothing Nothing, ".tif"))
  ,("gridTifGdal", (
       SagaCmd "libio_gdal" "2" ("GRIDS","FILE")
       (M.fromList [
           ]) Nothing Nothing, ".tif"))
  ,("gridXyz", (
         SagaCmd "libio_grid" "5" ("GRIDS","FILENAME")
         (M.fromList [
             ]) Nothing Nothing, ".xyz"))
  ,("polyDissolve", (
         SagaCmd "libshapes_polygons" "5" ("POLYGONS","DISSOLVED")
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
         SagaCmd "libta_morphometry" "0" ("ELEVATION","SLOPE")
         (M.fromList [
            ("aspect" ,("ASPECT" , "aspect"))
         ])
         Nothing Nothing, "_slope.sgrd"))
  ,("gridClassifyFlat", (
         SagaCmd "libgrid_tools" "15" ("INPUT","RESULT")
         (M.fromList [
            ("method" ,("METHOD" , "2")) -- 1: range
                                         -- 2: simple table
           ,("table" ,("RETAB" , "reclassify.txt"))
         ])
         (Just (\_ _ -> writeReclassifyTableFlatSlope "reclassify.txt"))
         (Just (\_ _ -> removeFile "reclassify.txt"))
                , "_reclassified.sgrd"))
  ,("gridClassToPoly", (
         SagaCmd "libshapes_grid" "6" ("GRID","POLYGONS")
         (M.fromList [
           ("id",    ("CLASS_ID" , "1")) -- class identifier
          ,("all",   ("CLASS_ALL" , "0")) -- 0: one single class specified by class identifier
                                          -- 1: all classes
          ,("split",  ("SPLIT" , "0")) -- 0: one single (multi-)polygon object
                                       -- 1: each island as separated polygon
         ])
         Nothing Nothing, "_polygons.shp"))
  ]

-- | Some common processing chains
sChainDB :: ChainDB
sChainDB = M.fromList [
    (("las"        , "grid")        , ["lasToPtCld","ptCldToGrid"])
   ,(("las"        , "grid-filled") , ["lasToPtCld","ptCldToGrid", "gridFillGaps"])
   ,(("las"        , "xyz-filled") , ["lasToPtCld","ptCldToGrid", "gridFillGaps", "gridXyz"])
   ,(("las"        , "hillshade")   , ["lasToPtCld","ptCldToGrid", "gridFillGaps", "gridHillshade"])
   ,(("las"        , "hillshade-tif")   , ["lasToPtCld","ptCldToGrid", "gridFillGaps", "gridHillshade", "gridTifHillshade"])
   ,(("las"        , "contour")     , ["lasToPtCld","ptCldToGrid", "gridFillGaps", "gridContour"])
   ,(("xyz-grid"   , "grid")        , ["xyzGridToGrid"])
   ,(("xyz-grid"   , "grid-filled") , ["xyzGridToGrid", "gridFillGaps"])
   ,(("xyz-grid"   , "xyz-filled")  , ["xyzGridToGrid", "gridFillGaps", "gridXyz"])
   ,(("xyz-grid"   , "hillshade")   , ["xyzGridToGrid", "gridFillGaps" , "gridHillshade"])
   ,(("xyz-grid"   , "hillshade-tif")   , ["xyzGridToGrid", "gridFillGaps" , "gridHillshade", "gridTifHillshade"])
   ,(("xyz-grid"   , "contour")     , ["xyzGridToGrid", "gridFillGaps" , "gridContour"])
   ,(("grid"       , "hillshade")   , ["gridFillGaps", "gridHillshade"])
   ,(("grid"       , "grid-filled") , ["gridFillGaps"])
   ,(("grid"       , "xyz-filled")  , ["gridFillGaps", "gridXyz"])
   ,(("grid"       , "hillshade")   , ["gridFillGaps", "gridHillshade"])
   ,(("grid"       , "hillshade-tif")   , ["gridFillGaps", "gridHillshade", "gridTifHillshade"])
   ,(("grid"       , "contour")     , ["gridFillGaps", "gridContour"])
   ,(("grid"       , "poly-clip")   , ["gridPolyClip"])
   ,(("grid-filled", "hillshade")   , ["gridHillshade"])
   ,(("grid-filled", "hillshade-tif"),["gridHillshade", "gridTifHillshade"])
   ,(("grid-filled", "contour")     , ["gridContour"])
   ,(("grid-filled", "xyz-filled")  , ["gridXyz"])
   ]


-- | Pathsway nodes with input and output commands
nodes :: [PathNode]
nodes =
  [
    PathNode "las" [] ["lasToPtCld"]
   ,PathNode "ptc" ["lasToPtCld"] ["ptCldToGrid"]
   ,PathNode "grid" ["ptCldToGrid"]
      ["gridFillGaps", "gridTifGdal", "gridPolyClip"
      ,"gridSlope", "gridClassToPoly", "gridClassifyFlat"]
   ,PathNode "xyz-grid" [] ["xyzGridToGrid"]
   ,PathNode "grid-filled" ["gridFillGaps"]
      ["gridHillshade", "gridXyz", "gridContour"]
   ,PathNode "grid-hillshade" ["gridHillshade"] ["gridTifHillshade"]
   ,PathNode "grid-hillshade-tif" ["gridTifHillshade"] []
   ,PathNode "grid-filled-xyz" ["gridXyz"] []
   ,PathNode "grid-filled-contour" ["gridContour"] []
   ,PathNode "grid-polygonClip" ["gridPolyClip"] []
  ]

