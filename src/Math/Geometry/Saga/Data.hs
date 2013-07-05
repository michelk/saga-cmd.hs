module Math.Geometry.Saga.Data where
import Math.Geometry.Saga.Types
import Math.Geometry.Saga.Utils 
import qualified Data.Map as M

-- | Implemeted saga-modules
sCmdDB :: CmdDB
sCmdDB = M.fromList [
    ("xyzGridToGrid", SagaCmd {
           sLib = "libio_grid", sMod = "6"
          ,sOutExt = ".sgrd" , sInOutKey = ("FILENAME","GRID")
          ,sParas = M.fromList [
               ("xyzCellSize" , ("CELLSIZE"  , "1"))
              ,("xyzSep"      , ("SEPARATOR" , "space"))
              ]
          ,sPre = nthn, sPost = nthn
          }
     )
    ,("gridFillGaps", SagaCmd {
           sLib = "libgrid_spline", sMod = "5"
          ,sOutExt = "_filled.sgrd" , sInOutKey = ("GRIDPOINTS","GRID_GRID")
          ,sParas = M.fromList [("gridFillTarget", ("TARGET", "1"))]
          ,sPre = \f -> copyGrid f (appendFileName f "_filled.sgrd")
          ,sPost = nthn
          }
     )
    ,("gridHillShade", SagaCmd {
           sLib = "libta_lighting", sMod = "0"
          ,sOutExt = "_hillshade.sgrd" , sInOutKey = ("ELEVATION","SHADE")
          ,sParas = M.fromList []
          ,sPre = nthn, sPost = nthn
          }
     )
    ,("gridContour", SagaCmd {
           sLib = "libshapes_grid", sMod = "5"
          ,sOutExt = "_contour.sgrd" , sInOutKey = ("INPUT","CONTOUR")
          ,sParas = M.fromList [
               ("contourMin" , ("ZMIN"  , "0"))
              ,("contourMax" , ("ZMAX" , "10000"))
              ,("contourStep" ,("ZSTEP" , "1"))
              ]
          ,sPre = nthn, sPost = nthn
          }
     )
    ,("lasToPtCld", SagaCmd {
           sLib = "libio_shapes_las", sMod = "1"
          ,sOutExt = ".pcl" , sInOutKey = ("POINTS","FILE")
          ,sParas = M.fromList [ ]
          ,sPre = nthn, sPost = nthn
          }
     )
    ,("ptCldToGrid", SagaCmd {
           sLib = "libpointcloud_tools", sMod = "4"
          ,sOutExt = ".sgrd" , sInOutKey = ("POINTS","GRID")
          ,sParas = M.fromList [ ]
          ,sPre = nthn, sPost = nthn
          }
     )
    ]
  where
    nthn _ = return ()

-- | Processing chains
sChainDB :: ChainDB
sChainDB = M.fromList [
    (("las", "grid"), ["lasToGrid"])
   ,(("las", "grid-filled"), ["lasToGrid", "gridFillGaps"])
   ,(("las", "hillshade"), ["lasToGrid", "gridFillGaps","gridHillShade"])
   ,(("las", "contour"), ["lasToGrid", "gridFillGaps", "gridContour"])
   ,(("xyz-grid", "grid"), ["xyzGridToGrid"])
   ,(("xyz-grid", "grid-filled"), ["xyzGridToGrid", "gridFillGaps"])
   ,(("xyz-grid", "hillshade"), ["xyzGridToGrid", "gridFillGaps", "gridHillShade"])
   ,(("xyz-grid", "contour"), ["xyzGridToGrid", "gridFillGaps", "gridContour"])
   ,(("grid",     "hillshade"), ["gridFillGaps", "gridHillShade"])
   ,(("grid",     "grid-filled"), ["gridFillGaps"])
   ,(("grid",     "hillshade"), ["gridFillGaps", "gridHillShade"])
   ,(("grid",     "contour"), ["gridFillGaps", "gridContour"])
   ,(("grid-filled","hillshade"), ["gridHillShade"])
   ,(("grid-filled","contour"), ["gridContour"])
   ]
