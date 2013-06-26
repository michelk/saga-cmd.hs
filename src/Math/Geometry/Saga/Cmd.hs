module Math.Geometry.Saga.Cmd where
import System.Cmd 
import GHC.IO.Exception
import System.Directory (copyFile)
import System.FilePath.Posix (dropExtension, replaceExtension)

-- | Actual Program to do the work
progName :: String
progName = "saga_cmd"

-- | Wrapper around saga
saga ::
       String               -- ^ Library name
    -> String               -- ^ Module name  
    -> [(String,String)]    -- ^ Parameter key-value (except of input)
    -> IO ExitCode          -- ^ Output-file
saga lib mod params = 
    system $ unwords [
        progName
       ,lib
       ,mod
       ,unwords $ renderParams params
       ]
  where
    renderParams = map renderPara
    renderPara (k,v) = "-" ++ k ++ " " ++ v

-- | Convert a xyz-grid to saga-grid 
xyzGridToGrid ::
       Double                   -- ^ Cellsize of grid
    -> String                   -- ^ Seperator
    -> FilePath                 -- ^ Input file-path
    -> IO FilePath              -- ^ Output file-path
xyzGridToGrid cs sep f = do
    result <-
        saga "libio_grid" "6"
        [
         ("GRID", outF)
        ,("CELLSIZE", show cs)
        ,("SEPARATOR", sepStr)
        ,("FILENAME", f)
        ]
    case result of
        ExitSuccess   -> return outF
        ExitFailure _ -> error  "saga_cmd failed"
    where
      outF = appendFileName f "_grid.sgrd"
      sepStr = dispSep sep

-- | Create a grid based on scatterd xyz-points
xyzToGrid :: 
       Double                   -- ^ Cellsize of grid
    -> String                   -- ^ Seperator
    -> FilePath                 -- ^ Input file-path
    -> IO FilePath              -- ^ Output file-path
xyzToGrid cs sep f = do
    result <- 
        saga "libio_grid" "6"
        [("FILENAME", f)
         ,("GRID", outF)
         ,("CELLSIZE", show cs)
         ,("SEPARATOR", sepStr)]
    case result of
        ExitSuccess   -> return outF
        ExitFailure _ -> error  "saga_cmd failed"
    where
      outF = appendFileName f "_grid.sgrd"
      sepStr = dispSep sep

-- | Fill Gaps in a grid 
gridFillGaps ::
    FilePath                    -- ^ Input-grid
    -> IO FilePath              -- ^ Output-grid
gridFillGaps f = do
    copyGrid f outF
    result <-
        saga "libgrid_spline" "5"
        [
            ("GRIDPOINTS",f)
           ,("TARGET","1")
           ,("GRID_GRID",outF)
        ]
    case result of
        ExitSuccess   -> return f
        ExitFailure _ -> error  "saga_cmd failed"
    where
      outF = appendFileName f "_filled.sgrd"

-- | Create a hillshade of grid
gridHillshade ::
    FilePath                    -- ^ Input-grid
    -> IO FilePath              -- ^ Ouput-grid
gridHillshade f = do
    result <-
        saga "libta_lighting" "0"
        [
            ("ELEVATION", f)
           ,("SHADE", outF)
        ]
    case result of
        ExitSuccess   -> return outF
        ExitFailure _ -> error  "saga_cmd failed"
    where
      outF = appendFileName f "_hillshade.sgrd"

-- | Create contour-lines of a grid
gridContour :: 
       Double             -- ^ minimum value
    -> Double             -- ^ maximum value
    -> Double                -- ^ vertical distance between contour-lines
    -> FilePath           -- ^ Input-grid
    -> IO FilePath        -- ^ Ouput-grid
gridContour min max d f = do
    result <-
        saga "libshapes_grid" "5"
        [
            ("INPUT", f)
           ,("CONTOUR", outF)
           ,("ZMIN", show min)
           ,("ZMIN", show max)
           ,("ZSTEP", show d)
        ]
    case result of
        ExitSuccess   -> return outF
        ExitFailure _ -> error  "saga_cmd failed"
    where
      outF = appendFileName f "_contour.shp"

-- | Laplacian filter (edge-detection)
gridFilterLaplace :: 
       FilePath                 -- ^ Input-grid
    -> IO FilePath              -- ^ Ouput-grid
gridFilterLaplace f = do
    result <-
        saga "libgrid_filter" "5"
        [
            ("GRID", f)
           ,("OUTPUT", outF)
        ]
    case result of
        ExitSuccess   -> return outF
        ExitFailure _ -> error  "saga_cmd failed"
    where
      outF = appendFileName f "_laplace.sgrd"

-- | grid Curvature
gridCurvature :: 
       FilePath                 -- ^ Input-grid
    -> IO FilePath              -- ^ Ouput-grid
gridCurvature f = do
    result <-
        saga "libta_morphometry" "0"
        [
            ("ELEVATION", f)
           ,("CURV", outF)
        ]
    case result of
        ExitSuccess   -> return outF
        ExitFailure _ -> error  "saga_cmd failed"
    where
      outF = appendFileName f "_curv.sgrd"

-- | grid Curvature
gridTif :: 
       FilePath                 -- ^ Input-grid
    -> IO FilePath              -- ^ Ouput-grid
gridTif f = do
    result <-
        saga "libio_gdal" "2"
        [
            ("GRIDS", f)
           ,("FILE", outF)
        ]
    case result of
        ExitSuccess   -> return outF
        ExitFailure _ -> error  "saga_cmd failed"
    where
      outF = appendFileName f ".tif"

-- | triangulation of raster-data
gridTin :: 
       String                   -- ^ Method to use ('Mark Highest Neighbour', 'Opposite Neighbours', 'Flow Direction', 'Flow Direction (up and down)', 'Peucker & Douglas')
    -> FilePath                 -- ^ Input-grid
    -> IO FilePath              -- ^ Ouput-grid
gridTin m f = do
    result <-
        saga "libtin_tools" "1"
        [
            ("GRID", f)
           ,("TIN", outF)
           ,("METHOD", m)
        ]
    case result of
        ExitSuccess   -> return outF
        ExitFailure _ -> error  "saga_cmd failed"
    where
      outF = appendFileName f ".tin"

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
