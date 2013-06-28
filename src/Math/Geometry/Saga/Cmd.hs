module Math.Geometry.Saga.Cmd where
import System.Cmd 
import Control.Monad ((>=>), foldM)
import GHC.IO.Exception
import System.Directory (copyFile)
import Data.Maybe (fromMaybe)
import System.FilePath.Posix (dropExtension, replaceExtension)
import qualified Data.Map as M


-- | A chain of saga_cmd calls, where each output-file gets piped as
--   input to follwing command
type CmdChain = [
    (String                     -- name of module-call
    ,[(String,String)])         -- Parameters associated to the module-call
    ]

-- | Execute a 'CmdChain'
doCmdChain ::    CmdDB          -- ^ Module data-base
              -> CmdChain       -- ^ sequence of commands with parameters
              -> FilePath       -- ^ Input-file
              -> IO FilePath    -- ^ Output-file
doCmdChain db chain fIn = undefined -- foldM >=> f convFuns

-- | Lookup a conversion function based on a module-name and parameters
lkpFunDB :: CmdDB                      -- ^ module-data-base
            -> String                  -- ^ module-name
            -> [(String,String)]       -- ^ parameters
            -> (FilePath -> IO String) -- ^ Conversion-function
lkpFunDB db k pars = undefined
  where
    cmd = fromMaybe
          (error $ k ++ " : Conversion function not supported")
          (M.lookup k db)

-- | Information needed to call saga_cmd
data SagaCmd = SagaCmd {
     sLib     :: String          -- ^ library to call
    ,sMod     :: String          -- ^ module number
    ,sOutExt  :: String          -- ^ extension to add to input-file
    ,sInOutKey:: (String,String) -- ^ Keys to use for input and output
    ,sParas   :: ParaMap -- ^ Parameters in addition to input and output -- 
    ,sPre     :: FilePath -> IO () -- ^ Pre-processing with input-file
    ,sPost    :: FilePath -> IO () -- ^ Post-processing with output-file
    }

-- | Parameter-Map
type ParaMap = [(Maybe String -- ^ Parameter-name accessable from the cmd-line
               ,String        -- ^ Parameter-name called in saga_cmd
               ,String        -- ^ Default-value to use
               )]
-- | Data-base for implemeted saga-modules
type CmdDB = M.Map
             String             -- ^ name of module-call
             SagaCmd            -- ^ configuration of module-call

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
          ,sPre = (\f -> copyGrid f (appendFileName f "_filled.sgrd"))
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
    nthn = (\_ -> return ())

doSaga :: SagaCmd -> FilePath -> IO String
doSaga (SagaCmd lib mod extOut (kIn, kOut) ps pre post) fIn = do
   pre fIn
   r <- saga lib mod ps'
   case r of
       ExitSuccess -> do
           post outF
           return outF
       ExitFailure _ -> error "saga_cmd failed"
   where
     outF = appendFileName fIn extOut
     ps' = (map (\(a,b,c) -> (b,c)) ps) ++ [(kIn,fIn),(kOut,outF)]
                             

              
-- | Actual Program to do the work
progName :: String
progName = "saga_cmd"

-- | Wrapper around saga
saga ::
       String                   -- ^ Library name
    -> String                   -- ^ Module name  
    -> [(String,String)]        -- ^ Parameter key-value
    -> IO ExitCode              -- ^ Output-file
saga lib mod params = 
    system $ unwords [
        progName
       ,lib
       ,mod
       ,unwords . renderParams  $ params
       ]
  where
    renderParams = map renderPara
    renderPara (k,v) = "-" ++ k ++ " " ++ v

-- -- | Convert a xyz-grid to saga-grid 
-- xyzGridToGrid ::
--        Double                   -- ^ Cellsize of grid
--     -> String                   -- ^ Seperator
--     -> FilePath                 -- ^ Input file-path
--     -> IO FilePath              -- ^ Output file-path
-- xyzGridToGrid cs sep f = do
--     r <-
--         saga "libio_grid" "6"
--         [
--          ("GRID", outF)
--         ,("CELLSIZE", show cs)
--         ,("SEPARATOR", sepStr)
--         ,("FILENAME", f)
--         ]
--     dispatchResult outF  r
--     where
--       sepStr = dispSep sep
--       outF = appendFileName f "_grid.sgrd"
-- 
--       
-- -- | Create a grid based on scatterd xyz-points
-- xyzToGrid :: 
--        Double                   -- ^ Cellsize of grid
--     -> String                   -- ^ Seperator
--     -> FilePath                 -- ^ Input file-path
--     -> IO FilePath              -- ^ Output file-path
-- xyzToGrid cs sep f = do
--     r <- 
--         saga "libio_grid" "6"
--         [("FILENAME", f)
--          ,("GRID", outF)
--          ,("CELLSIZE", show cs)
--          ,("SEPARATOR", sepStr)]
--     dispatchResult outF r
--     where
--       sepStr = dispSep sep
--       outF = appendFileName f "_grid.sgrd"
-- 
-- -- | Fill Gaps in a grid 
-- gridFillGaps ::
--     FilePath                    -- ^ Input-grid
--     -> IO FilePath              -- ^ Output-grid
-- gridFillGaps f = do
--     copyGrid f outF
--     r <-
--         saga "libgrid_spline" "5"
--         [
--             ("GRIDPOINTS",f)
--            ,("TARGET","1")
--            ,("GRID_GRID",outF)
--         ]
--     dispatchResult outF r
--     where
--       outF = appendFileName f "_filled.sgrd"
-- 
-- -- | Create a hillshade of grid
-- gridHillshade ::
--     FilePath                    -- ^ Input-grid
--     -> IO FilePath              -- ^ Ouput-grid
-- gridHillshade f = do
--     r <-
--         saga "libta_lighting" "0"
--         [
--             ("ELEVATION", f)
--            ,("SHADE", outF)
--         ]
--     dispatchResult outF r
--     where
--       outF = appendFileName f "_hillshade.sgrd"
-- 
-- -- | Create contour-lines of a grid
-- gridContour :: 
--        Double             -- ^ minimum value
--     -> Double             -- ^ maximum value
--     -> Double                -- ^ vertical distance between contour-lines
--     -> FilePath           -- ^ Input-grid
--     -> IO FilePath        -- ^ Ouput-grid
-- gridContour min max d f = do
--     r <-
--         saga "libshapes_grid" "5"
--         [
--             ("INPUT", f)
--            ,("CONTOUR", outF)
--            ,("ZMIN", show min)
--            ,("ZMIN", show max)
--            ,("ZSTEP", show d)
--         ]
--     dispatchResult outF r
--     where
--       outF = appendFileName f "_contour.shp"
-- 
-- -- | Laplacian filter (edge-detection)
-- gridFilterLaplace :: 
--        FilePath                 -- ^ Input-grid
--     -> IO FilePath              -- ^ Ouput-grid
-- gridFilterLaplace f = do
--     r <-
--         saga "libgrid_filter" "5"
--         [
--             ("GRID", f)
--            ,("OUTPUT", outF)
--         ]
--     dispatchResult outF r
--     where
--       outF = appendFileName f "_laplace.sgrd"
-- 
-- -- | grid Curvature
-- gridCurvature :: 
--        FilePath                 -- ^ Input-grid
--     -> IO FilePath              -- ^ Ouput-grid
-- gridCurvature f = do
--     r <-
--         saga "libta_morphometry" "0"
--         [
--             ("ELEVATION", f)
--            ,("CURV", outF)
--         ]
--     dispatchResult outF r
--     where
--       outF = appendFileName f "_curv.sgrd"
-- 
-- -- | grid Curvature
-- gridTif :: 
--        FilePath                 -- ^ Input-grid
--     -> IO FilePath              -- ^ Ouput-grid
-- gridTif f = do
--     r <-
--         saga "libio_gdal" "2"
--         [
--             ("GRIDS", f)
--            ,("FILE", outF)
--         ]
--     dispatchResult outF r
--     where
--       outF = appendFileName f ".tif"
-- 
-- -- | triangulation of raster-data
-- gridTin :: 
--        String                   -- ^ Method to use ('Mark Highest Neighbour', 'Opposite Neighbours', 'Flow Direction', 'Flow Direction (up and down)', 'Peucker & Douglas')
--     -> FilePath                 -- ^ Input-grid
--     -> IO FilePath              -- ^ Ouput-grid
-- gridTin m f = do
--     r <-
--         saga "libtin_tools" "1"
--         [
--             ("GRID", f)
--            ,("TIN", outF)
--            ,("METHOD", m)
--         ]
--     dispatchResult outF r
--     where
--       outF = appendFileName f ".tin"

-- | Copy a grid data-set
copyGrid :: FilePath -> FilePath -> IO ()
copyGrid f t = mapM_ cp ["sgrd", "sdat", "mgrd"]
   where
     cp ext = copyFile (replaceExtension f ext) (replaceExtension t ext)
    
-- | Utility function to append to basename of a file-name
appendFileName :: FilePath -> String -> FilePath
appendFileName f s = dropExtension f ++ s

-- | Dispatch on field seperator
dispSep :: String -> String
dispSep s = case s of
    "space"     -> "space"
    "Space"     -> "space"
    " "         -> "space"
    "\t"        -> "tabulator"
    "tab"       -> "tabulator"
    "Tab"       -> "tabulator"
    "Tabulator" -> "tabulator"
    ";"         -> ";"
    ","         -> ","

-- | Dispatch return-value; if saga_cmd failed, raise an errer
--   otherwise return the output filepath
dispatchResult ::
       FilePath                 -- ^ output-file-name
    -> ExitCode                 -- ^ saga_cmd return value
    -> IO String                -- ^ Ouput-file-name
dispatchResult outF r = case r of
    ExitSuccess   -> return outF
    ExitFailure _ -> error  "saga_cmd failed"
