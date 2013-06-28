module Math.Geometry.Saga.Modules where
import Math.Geometry.Saga.Types
import Math.Geometry.Saga.Utils 
import qualified Data.Map as M

-- | Implemeted saga-modules
myCmdDB :: CmdDB
myCmdDB = M.fromList [
    ("xyzGridToGrid", SagaCmd {
           sLib = "libio_grid", sMod = "6"
          ,sOutExt = ".sgrd" , sInOutKey = ("FILENAME","GRID")
          ,sParas = [
               (Just "xyzCellSize" , "CELLSIZE"  , "1")
              ,(Just "xyzSep"      , "SEPARATOR" , "space")
              ]
          ,sPre = nthn, sPost = nthn
          }
     )
    ,("gridFillGaps", SagaCmd {
           sLib = "libgrid_spline", sMod = "5"
          ,sOutExt = "_filled.sgrd" , sInOutKey = ("GRIDPOINTS","GRID_GRID")
          ,sParas = [(Nothing, "TARGET", "1")]
          ,sPre = \f -> copyGrid f (appendFileName f "_filled.sgrd")
          ,sPost = nthn
          }
     )
    ,("gridHillShade", SagaCmd {
           sLib = "libta_ligthning", sMod = "0"
          ,sOutExt = "_hillshade.sgrd" , sInOutKey = ("ELEVATION","SHADE")
          ,sParas = []
          ,sPre = nthn, sPost = nthn
          }
     )
    ]
  where
    nthn _ = return ()
