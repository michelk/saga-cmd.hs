{-# LANGUAGE DeriveDataTypeable #-}
-- | Program to 
module Main where
import           Control.Monad (when)
import qualified Data.Map as M
import           Math.Geometry.Saga.Cmd
import           Math.Geometry.Saga.Data
import           Math.Geometry.Saga.LUT
import           Math.Geometry.Saga.Types
import           Prelude hiding (min,max)
import           System.Console.CmdArgs
import           System.Environment (getArgs, withArgs)

_PROGRAM_NAME, _PROGRAM_VERSION, _PROGRAM_INFO, _PROGRAM_ABOUT, _COPYRIGHT :: String
_PROGRAM_NAME    = "sagaTopo"
_PROGRAM_VERSION = "0.0.0.1"
_PROGRAM_INFO    = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_COPYRIGHT       = "GPL licensed; written by Michel Kuhlmann 2013"
_PROGRAM_ABOUT   = "Create create topographic maps out of gridded data"

_COLOR_FILE :: FilePath
_COLOR_FILE = "relief-colors.txt"

main :: IO ()
main = do
    cArgs <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null cArgs then withArgs ["--help"] else id) (cmdArgs defaultOpts)
    when (output opts == "") (fail "Please specify an output-tif")
    r <- case (min opts, max opts) of
      (0.0, 0.0) -> sgrdToTopo (file opts) (output opts) Nothing Nothing
      _          -> sgrdToTopo (file opts) (output opts)
                        (Just $ min opts) (Just $ max opts)
    putStrLn ("Successfully created " ++ r)

-- | Convert a grid-file into a topo graphic map
sgrdToTopo :: FilePath -> FilePath -> Maybe Float -> Maybe Float -> IO FilePath
sgrdToTopo fIn fOut Nothing Nothing = do
  let hsF = "temp_hillshade.sgrd"
  doSaga $ gridHillShade hsF fIn
  doSaga $ SagaCmd "libio_grid_image" "0" ("GRID", "FILE")
          (M.fromList [
              ("a",("SHADE", hsF))
              ])
          Nothing Nothing fOut fIn

sgrdToTopo fIn fOut (Just min') (Just max') = do
  let hsF = "temp_hillshade.sgrd"
  doSaga $ gridHillShade hsF fIn
  writeFile _COLOR_FILE (bgrColTable min' max') -- color-lookup-table
  doSaga $ SagaCmd "libio_grid_image" "0" ("GRID", "FILE")
          (M.fromList [
              ("a",("SHADE", hsF))
             ,("b", ("LUT", _COLOR_FILE))
              ])
          Nothing Nothing fOut fIn

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
    {output = def &= help "output-tif"
    ,min    = def &= help "minimum elevation-value (optional)"
    ,max    = def &= help "maximum elevation-value (optional)"
    ,file        = def &= args &= typ "DEM-file.sgrd"
    } &=
    program _PROGRAM_NAME &=
    help _PROGRAM_ABOUT &=
    summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)

gridHillShade :: SagaIoCmd
gridHillShade = fst $ lkpCmd "gridHillShade" sIoDB

