module Math.Geometry.Saga.Data where
import Math.Geometry.Saga.Types
import Math.Geometry.Saga.Utils
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
       (M.fromList []) Nothing Nothing , "_filled.sgrd"))
  ,("ptCldToGrid", (
       SagaCmd "libpointcloud_tools" "4" ("POINTS","GRID")
       (M.fromList []) Nothing Nothing ,"_hillshade.sgrd"))
  ,("gridFillGaps", (
       SagaCmd "libgrid_spline" "5" ("GRIDPOINTS","GRID_GRID")
       (M.fromList [("grdFlT", ("TARGET", "1"))])
       (Just copyGrid) Nothing, "_contour.sgrd"))
  ,("gridHillShade", (
       SagaCmd "libta_lighting" "0" ("ELEVATION","SHADE")
       (M.fromList []) Nothing Nothing , "_polyClip.sgrd"))
  ,("gridContour", (
       SagaCmd "libshapes_grid" "5" ("INPUT","CONTOUR")
       (M.fromList [
            ("min" , ("ZMIN"  , "0"))
           ,("max" , ("ZMAX" , "10000"))
           ,("d",    ("ZSTEP" , "1"))
           ]) Nothing Nothing, ".pcl"))
  ,("gridPolyClip", (
       SagaCmd "libshapes_grid" "7" ("INPUT","OUTPUT")
       (M.fromList [
           ("poly" , ("POLYGONS"  , ""))
           ]) Nothing Nothing, ".sgrd"))
  ]

-- | Some common processing chains
sChainDB :: ChainDB
sChainDB = M.fromList [
    (("las"        , "grid")        , ["lasToPtCld","ptCldToGrid"])
   ,(("las"        , "grid-filled") , ["lasToPtCld","ptCldToGrid", "gridFillGaps"])
   ,(("las"        , "hillshade")   , ["lasToPtCld","ptCldToGrid", "gridFillGaps", "gridHillShade"])
   ,(("las"        , "contour")     , ["lasToPtCld","ptCldToGrid", "gridFillGaps", "gridContour"])
   ,(("xyz-grid"   , "grid")        , ["xyzGridToGrid"])
   ,(("xyz-grid"   , "grid-filled") , ["xyzGridToGrid", "gridFillGaps"])
   ,(("xyz-grid"   , "hillshade")   , ["xyzGridToGrid", "gridFillGaps" , "gridHillShade"])
   ,(("xyz-grid"   , "contour")     , ["xyzGridToGrid", "gridFillGaps" , "gridContour"])
   ,(("grid"       , "hillshade")   , ["gridFillGaps", "gridHillShade"])
   ,(("grid"       , "grid-filled") , ["gridFillGaps"])
   ,(("grid"       , "hillshade")   , ["gridFillGaps", "gridHillShade"])
   ,(("grid"       , "contour")     , ["gridFillGaps", "gridContour"])
   ,(("grid"       , "poly-clip")   , ["gridPolyClip"])
   ,(("grid-filled", "hillshade")   , ["gridHillShade"])
   ,(("grid-filled", "contour")     , ["gridContour"])
   ]
