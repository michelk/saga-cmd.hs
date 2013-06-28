module Math.Geometry.Saga.Cmd where
import Math.Geometry.Saga.Types
import Math.Geometry.Saga.Utils
import System.Cmd (system)
import GHC.IO.Exception

-- | Actual Program to do the work
progName :: String
progName = "saga_cmd"

 -- | Call saga with a specific configuration
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
     ps' = map (\(a,b,c) -> (b,c)) ps ++ [(kIn,fIn),(kOut,outF)]
              

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

