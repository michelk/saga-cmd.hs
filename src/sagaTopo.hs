{-# LANGUAGE DeriveDataTypeable #-}
-- | Program to 
module Main where
import           Prelude hiding (min,max)
import           System.Console.CmdArgs
import           System.Environment (getArgs, withArgs)

_PROGRAM_NAME    = "sagaTopo"
_PROGRAM_VERSION = "0.0.0.1"
_PROGRAM_INFO    = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_COPYRIGHT       = "GPL licensed; written by Michel Kuhlmann 2013"
_PROGRAM_ABOUT   = "Create create topographic maps out of gridded data"
_PROGRAM_DETAILS = ["Program dispatches on input and output file extensions;"
                   , "input-file: .las,.xyz.sgrd,.tif"]

main :: IO ()
main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) (cmdArgs defaultOpts)
    putStrLn "bla bla"

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
    ,file        = def &= args &= typ "DEM-file"
    } &=
    program _PROGRAM_NAME &=
    help _PROGRAM_ABOUT &=
    summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT) &=
    details _PROGRAM_DETAILS
