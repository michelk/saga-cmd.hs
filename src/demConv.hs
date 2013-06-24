{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import System.Console.CmdArgs
import Control.Monad (when)
import System.Environment (getArgs, withArgs)

_PROGRAM_NAME    = "demConv"
_PROGRAM_VERSION = "0.0.0.1"
_PROGRAM_INFO    = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT   = "Convert Digital Elevation Models (DEM)"
_COPYRIGHT       = "GPL licensed; written by Michel Kuhlmann 2013"

-- | Data structure for command line options.
data Opt = Opt
    { 
      from   :: String          -- ^ format to convert from
    , to     :: String          -- ^ format to convert into
    , params :: String -- ^ Parameters to pass into the different conversion steps
    , file   :: FilePath        -- ^ Command-line arguments
    } deriving (Show, Data, Typeable)

-- | Parameters to use for the diffenrent conversion-steps
data Params = Params {
    xyzSep      :: String       -- ^ Seperator, when reading xyz-data
   ,xyzCellSize :: Double       -- ^ target grid-cell-size
   ,contourDiff :: Double -- ^ difference in elevation between contour-lines
   } deriving (Show)

-- | Conversion Data-base
type ConvDB =
    [(String                 -- ^ Source Format
     ,String                 -- ^ Target Format
     ,String -> IO String    -- ^ Conversion-function
     )]

-- | Defaults for command-line options.
defaultOpts :: Opt
defaultOpts = Opt
    { 
      from   = def &= help "Source-format; currently: xyz,xyz-grid,grid"
    , to     = def &= help "Target-format; currently: contour,hillshade,grid-filled"
    , params = def &= help "Parameters to pass into the different conversion steps"
    , file  = def &= args &= typ "DEM-file"
    } &=
    program _PROGRAM_NAME &=
    help _PROGRAM_ABOUT &=
    summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT) 

-- | Default conversion-parameters
defaultParams :: Params
defaultParams = Params {
    xyzSep      = "space"
   ,xyzCellSize = 1.0
   ,contourDiff = 1.0               
   }

-- | Convsersion data-base
myConvDB :: Params -> ConvDB
myConvDB p = [
    ("xyz-grid"    , "grid"        , xyzGridToGrid cs sep)
   ,("xyz-grid"    , "grid-filled" , xyzGridToFilledGrid)
   ,("xyz-grid"    , "hillshade"   , xyzGridToFilledGrid >>= gridHillshade)
   ,("grid"        , "hillshade"   , gridFillGaps >>= gridHillshade)
   ,("grid-filled" , "hillshade"   , gridHillshade)
   ,("xyz-grid"    , "contour"     , xyzGridToFilledGrid >>= gridContour)
   ,("grid"        , "contour"     , gridFillGaps >>= gridContour)
   ,("grid-filled" , "contour"     , gridContour)
    ]
  where
    sep                 = xyzSep p
    cs                  = xyzCellSize p
    xyzGridToFilledGrid = xyzToGrid cs sep >>= gridFillGaps

main :: IO ()
main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) (cmdArgs defaultOpts)
    let f = dispatchReaderWriter (from opts) (to opts)
    TIO.interact f
