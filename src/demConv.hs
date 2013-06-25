{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import System.Console.CmdArgs
import Math.Geometry.Saga.Cmd
import Control.Monad (when)
import Data.Text (split, pack, unpack, Text)
import qualified Data.HashMap.Lazy as M
import Data.Maybe (fromMaybe)
import System.Environment (getArgs, withArgs)

main :: IO ()
main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) (cmdArgs defaultOpts)
    putStr (parameters opts)

_PROGRAM_NAME    = "demConv"
_PROGRAM_VERSION = "0.0.0.1"
_PROGRAM_INFO    = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT   = "Convert Digital Elevation Models (DEM) to diffent formats"
_COPYRIGHT       = "GPL licensed; written by Michel Kuhlmann 2013"

-- | Data structure for command line options.
data Opt = Opt
    { 
      from       :: String      -- ^ format to convert from
    , to         :: String      -- ^ format to convert into
    , parameters :: String -- ^ Parameters to pass into the different conversion steps
    , file       :: FilePath    -- ^ Command-line arguments
    } deriving (Show, Data, Typeable)

-- | Parameters to use for the diffenrent conversion-steps
type Params = M.HashMap String String

-- | Conversion Data-base
type ConvDB =
    [(String                    -- ^ Source Format
     ,String                    -- ^ Target Format
     ,String -> IO String       -- ^ Conversion-function
     )]

-- | Defaults for command-line options.
defaultOpts :: Opt
defaultOpts = Opt
    { 
      from        = def &= help "Source-format; currently: xyz,xyz-grid,grid"
    , to          = def &= help "Target-format; currently: contour,hillshade,grid-filled"
    , parameters  = def &= help "Parameters to pass into the different conversion steps"
    , file        = def &= args &= typ "DEM-file"
    } &=
    program _PROGRAM_NAME &=
    help _PROGRAM_ABOUT &=
    summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT) 

-- | Default conversion-parameters
defaultParams :: Params
defaultParams = M.fromList [
    ("xyzSep", "space")
   ,("xyzCellSize", "1")
   ,("contourDiff", "1")
   ,("tinMethod",  "Opposite Neighbours")
   ]

-- | Convsersion data-base
myConvDB :: Params -> ConvDB
myConvDB p = [
    ("xyz-grid"    , "grid"        , xyzGridToGrid cs sep)
   ,("xyz-grid"    , "grid-filled" , xyzGridToFilledGrid)
   ,("xyz-grid"    , "hillshade"   , xyzGridToHillShade)
   ,("grid"        , "hillshade"   , gridToHillShade )
   ,("grid-filled" , "hillshade"   , gridHillshade)
   ,("xyz-grid"    , "contour"     , xyzGridToContour)
   ,("grid"        , "contour"     , gridToContour)
   ,("grid-filled" , "contour"     , gridContour d)
   ,("xyz-grid"    , "tif"         , xyzGridToTif)
   ,("grid"        , "tif"         , gridToTif)
   ,("grid-filled" , "tif"         , gridTif)
    ]
  where
    sep                   = fromMaybe $ lookup "xyzSep" p
    cs                    = fromMaybe $ lookup "xyzCellSize" p
    d                     = fromMaybe $ lookup "contourDiff" p
    xyzGridToFilledGrid f = xyzToGrid cs sep f >>= gridFillGaps
    xyzGridToHillShade f  = xyzGridToFilledGrid f >>= gridHillshade
    gridToHillShade f     = gridFillGaps f >>= gridHillshade
    xyzGridToContour f    = xyzGridToFilledGrid f >>= gridContour d
    gridToContour f       = gridFillGaps f >>= gridContour d
    xyzGridToTif f        = xyzGridToFilledGrid f >>= gridTif
    gridToTif f           = gridFillGaps f >>= gridTif

-- | Parse the command-line string specifying parameters
parseParamCmdString :: String -> [(String,String)]
parseParamCmdString s = map parseAssign $ splitStr ':' s
  where
    parseAssign :: String -> (String,String)
    parseAssign s = let k:v:[] = splitStr '=' s in (k,v)

-- | Overwrite the default Parameters
adjustDefaultParams :: Params -> [(String,String)] -> Params
adjustDefaultParams p kvs = foldr f p kvs
  where
    f :: (String,String) -> Params -> Params
    f (k,v) pAcc = undefined

-- | Split a String on a certain delimiter
splitStr :: Char -> String -> [String]
splitStr c s = map unpack $ split (== c) (pack s)

