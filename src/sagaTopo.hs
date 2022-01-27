{-# LANGUAGE DeriveDataTypeable #-}

-- | Program to
module Main where

import Control.Monad (when)
import qualified Data.Map as M
import Gis.Saga.Cmd
import Gis.Saga.Data
import Gis.Saga.LUT
import Gis.Saga.Types
import System.Console.CmdArgs
import System.Environment
  ( getArgs,
    withArgs,
  )
import Prelude hiding
  ( max,
    min,
  )

_PROGRAM_NAME,
  _PROGRAM_VERSION,
  _PROGRAM_INFO,
  _PROGRAM_ABOUT,
  _COPYRIGHT ::
    String
_PROGRAM_NAME = "sagaTopo"
_PROGRAM_VERSION = "0.1.0.0"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_COPYRIGHT = "GPL licensed; written by Michel Kuhlmann 2013"
_PROGRAM_ABOUT = "Create create topographic maps out of gridded data"

_COLOR_FILE :: FilePath
_COLOR_FILE = "relief-colors.txt"

main :: IO ()
main = do
  cArgs <- getArgs
  -- If the user did not specify any arguments, pretend as "--help" was given
  opts <- (if null cArgs then withArgs ["--help"] else id) (cmdArgs defaultOpts)
  when (output opts == "") (fail "Please specify an output-tif")
  r <- case (hillshade opts, min opts, max opts) of
    (True, 0.0, 0.0) -> sgrdToTopo (file opts) (output opts) Nothing Nothing
    (False, 0.0, 0.0) -> sgrdToRelief (file opts) (output opts) Nothing Nothing
    (True, _, _) ->
      sgrdToTopo (file opts) (output opts) (Just $ min opts) (Just $ max opts)
    (False, _, _) ->
      sgrdToRelief (file opts) (output opts) (Just $ min opts) (Just $ max opts)
  putStrLn ("Successfully created " ++ r)

-- | Convert a grid-file into a topo graphic map
sgrdToTopo :: FilePath -> FilePath -> Maybe Float -> Maybe Float -> IO FilePath
sgrdToTopo fIn fOut Nothing Nothing = do
  let hsF = "temp_hillshade.sgrd"
  doSaga False (gridHillShade hsF fIn)
  doSaga False $
    SagaCmd
      "libio_grid_image"
      "0"
      ("GRID", "FILE")
      (M.fromList [("a", ("SHADE", hsF))])
      Nothing
      Nothing
      fOut
      fIn
sgrdToTopo fIn fOut (Just min') (Just max') = do
  let hsF = "temp_hillshade.sgrd"
  doSaga False $ gridHillShade hsF fIn
  writeFile _COLOR_FILE (bgrColTable min' max') -- color-lookup-table
  doSaga False $
    SagaCmd
      "libio_grid_image"
      "0"
      ("GRID", "FILE")
      (M.fromList [("a", ("SHADE", hsF)), ("b", ("LUT", _COLOR_FILE))])
      Nothing
      Nothing
      fOut
      fIn

sgrdToRelief ::
  FilePath -> FilePath -> Maybe Float -> Maybe Float -> IO FilePath
sgrdToRelief fIn fOut Nothing Nothing = do
  doSaga False $
    SagaCmd
      "libio_grid_image"
      "0"
      ("GRID", "FILE")
      M.empty
      Nothing
      Nothing
      fOut
      fIn
sgrdToRelief fIn fOut (Just min') (Just max') = do
  writeFile _COLOR_FILE (bgrColTable min' max') -- color-lookup-table
  doSaga False $
    SagaCmd
      "libio_grid_image"
      "0"
      ("GRID", "FILE")
      (M.fromList [("b", ("LUT", _COLOR_FILE))])
      Nothing
      Nothing
      fOut
      fIn

-- | Data structure for command line options.
data Opt = Opt
  { min :: Float,
    max :: Float,
    output :: FilePath,
    hillshade :: Bool,
    file :: FilePath
  }
  deriving (Show, Data, Typeable)

-- | Defaults for command-line options.
defaultOpts :: Opt
defaultOpts =
  Opt
    { output = def &= typ "TIF" &= help "output-tif",
      min = def &= help "minimum elevation-value (optional)",
      max = def &= help "maximum elevation-value (optional)",
      hillshade = def &= help "Should a hillshade be included",
      file = def &= args &= typ "DEM-file.sgrd"
    }
    &= program _PROGRAM_NAME
    &= help _PROGRAM_ABOUT
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)

gridHillShade :: SagaIoCmd
gridHillShade = fst $ lkpCmd "gridHillshade" sIoDB
