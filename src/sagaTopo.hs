{-# LANGUAGE DeriveDataTypeable #-}
-- | Program to 
module Main where
import qualified Data.Map as M
import           Math.Geometry.Saga.Cmd
import           Math.Geometry.Saga.Data
import           Math.Geometry.Saga.LUT
import           Math.Geometry.Saga.Types
import           Math.Geometry.Saga.Utils
import           Prelude hiding (min,max)
import           System.Console.CmdArgs
import           System.Directory (renameFile)
import           System.Environment (getArgs, withArgs)

_PROGRAM_NAME    = "sagaTopo"
_PROGRAM_VERSION = "0.0.0.1"
_PROGRAM_INFO    = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_COPYRIGHT       = "GPL licensed; written by Michel Kuhlmann 2013"
_PROGRAM_ABOUT   = "Create create topographic maps out of gridded data"
_PROGRAM_DETAILS = ["Program dispatches on input and output file extensions;"
                   , "input-file: .las,.xyz.sgrd,.tif"]

_COLOR_FILE = "relief-colors.txt"
main :: IO ()
main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) (cmdArgs defaultOpts)
    r <- sgrdToTopo (file opts) (output opts) (min opts) (max opts)
    putStrLn ("Successfully created" ++ r)
    
-- | Convert a grid-file into a topo graphic map
sgrdToTopo :: FilePath -> FilePath -> Float -> Float -> IO FilePath
sgrdToTopo fIn fOut min max = do
  hsF <- hsFun fIn              -- ^ filepath to hillshade
  writeFile _COLOR_FILE (bgrColTable min max) -- color-lookup-table
  sOut <- doSaga SagaCmd {
                      sLib = "libio_grid_image"
                     ,sMod = "0"
                     ,sOutExt = ".tif"
                     ,sInOutKey = ("GRID", "FILE")
                     ,sParas = M.fromList [
                       ("tifShade",("SHADE", hsF))
                       ,("tifLut", ("LUT", _COLOR_FILE))
                       ]
                     ,sPre = \_ -> return ()
                     ,sPost = \_ -> return ()}
          fIn
  renameFile sOut fOut
  return fOut
  where
    hsFun = lkpFunDB sCmdDB (M.fromList []) "gridHillShade"

-- | Convert a grid-file into contour-lines
sgrdToContour :: FilePath -> FilePath -> Float ->Float -> Float -> IO FilePath
sgrdToContour fIn fOut min max d = undefined

-- | Data structure for command line options.
data Opt = Opt
    {min    :: Float
    ,max    :: Float
    ,output :: FilePath
    ,file   :: FilePath
    } deriving (Show, Data, Typeable)

-- | Defaults for command-line options.
defaultOpts :: Opt
defaultOpts = Opt
    {output = def &= help "output-file-name (supported extension: .tif,.png,.shp (contour-lines))"
    ,min    = def &= help "minimum elevation-value (optional)"
    ,max    = def &= help "maximum elevation-value (optional)"
    ,file        = def &= args &= typ "DEM-file.sgrd"
    } &=
    program _PROGRAM_NAME &=
    help _PROGRAM_ABOUT &=
    summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT) &=
    details _PROGRAM_DETAILS
