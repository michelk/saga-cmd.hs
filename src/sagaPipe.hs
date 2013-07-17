{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import System.Console.CmdArgs
import Math.Geometry.Saga.Types
import Math.Geometry.Saga.Data
import Math.Geometry.Saga.Utils
import Math.Geometry.Saga.Cmd
import Data.Text (split, pack, unpack, Text)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import System.Environment (getArgs, withArgs)

_PROGRAM_NAME, _PROGRAM_VERSION, _PROGRAM_INFO, _PROGRAM_ABOUT, _COPYRIGHT :: String
_PROGRAM_NAME    = "sagaChain"
_PROGRAM_VERSION = "0.0.1.0"
_PROGRAM_INFO    = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_COPYRIGHT       = "GPL licensed; written by Michel Kuhlmann 2013"
_PROGRAM_ABOUT   = "Convert Digital Elevation Models (DEM) to diffent formats"


main :: IO ()
main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) (cmdArgs defaultOpts)
    let cmdPars = parseParamCmdString $ parameters opts
        chain = fromMaybe (error "from-to-combination not supported")
                          (M.lookup (from opts, to opts) sChainDB)
    result <- doCmdChain chain cmdPars (file opts)
    putStrLn ("Succussfully created " ++ result ) 

-- | Data structure for command line options.
data Opt = Opt
    { 
      from       :: String      -- ^ format to convert from
    , to         :: String      -- ^ format to convert into
    , parameters :: String -- ^ Parameters to pass into the different conversion steps
    , file       :: FilePath    -- ^ Command-line arguments
    } deriving (Show, Data, Typeable)

-- | Defaults for command-line options.
defaultOpts :: Opt
defaultOpts = Opt
    { 
      from        = def &= help "Source-format"
    , to          = def &= help "Target-format"
    , parameters  = def &= help "Parameters to pass into the different conversion steps, delimited by ':'(eg xyzCellSize=0.5:xyzSep=tabulator)"
    , file        = def &= args &= typ "DEM-input-file"
    } &=
    program _PROGRAM_NAME &=
    help _PROGRAM_ABOUT &=
    summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT) 

-- | Parse the command-line string specifying parameters
parseParamCmdString :: String -> CmdPars
parseParamCmdString "" = M.empty
parseParamCmdString s = M.fromList . map parseAssign . splitStr ':' $ s
  where
    parseAssign :: String -> (String,String)
    parseAssign s = let k:v:[] = splitStr '=' s in (k,v)

-- | Split a String on a certain delimiter
splitStr :: Char -> String -> [String]
splitStr c s = map unpack $ split (== c) (pack s)

fromTos :: String
fromTos = renderStringPairs (M.keys sChainDB)
