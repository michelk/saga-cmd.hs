{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Control.Monad (when)
import System.Console.CmdArgs
import Math.Geometry.Saga.Types
import Math.Geometry.Saga.Data
import Math.Geometry.Saga.Utils
import Math.Geometry.Saga.Cmd
import Math.Geometry.Saga.Doc (renderTable)
import Data.Text (split, pack, unpack, Text)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import System.Environment (getArgs, withArgs)
import System.Exit (exitSuccess)
_PROGRAM_NAME, _PROGRAM_VERSION, _PROGRAM_INFO, _PROGRAM_ABOUT, _COPYRIGHT :: String
_PROGRAM_NAME    = "sagaPipe"
_PROGRAM_VERSION = "0.0.1.0"
_PROGRAM_INFO    = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_COPYRIGHT       = "GPL licensed; written by Michel Kuhlmann 2013"
_PROGRAM_ABOUT   = "Convert Digital Elevation Models (DEM) to diffent formats"


main :: IO ()
main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) (cmdArgs defaultOpts)
    when (modules opts) (sequence_ [putStrLn (renderTable sIoDB), exitSuccess])
    when (null $ file opts) (error "Please specify an input-file")
    let cmdPars = parseParamCmdString $ parameters opts
        cmdChain = case chain opts of
          "" -> lkpChain sIoDB (fromMaybe (error "from-to-combination not supported")
                                          (M.lookup (from opts, to opts) sChainDB))
          _ -> lkpChain  sIoDB (splitStr ':' $ chain opts)
    result <- doCmdChain cmdChain cmdPars (file opts) (case (output opts) of
                                                          "" -> Nothing
                                                          _  -> Just  (output opts)
                                                      )
    putStrLn ("Succussfully created " ++ result )

-- | Data structure for command line options.
data Opt = Opt
    {
      from       :: String
    , to         :: String
    , parameters :: String
    , chain      :: String
    , output     :: FilePath
    , modules    :: Bool
    , file       :: FilePath
    } deriving (Show, Data, Typeable)

-- | Defaults for command-line options.
defaultOpts :: Opt
defaultOpts = Opt
    {
      from       = def &= help "Source-format"
    , to         = def &= help "Target-format"
    , parameters = def &= help "Conversion-Parameters; delimited by ':'(eg cs=0.5:sep=tabulator)"
    , chain      = def &= help "Conversion-pathway; delimited by ':'(eg cXyzGridToGrid:cGridFillGaps)"
    , output     = def &= help "Output-file (optional)"
    , modules    = def &= help "Create a list of available modules"
    , file       = def &= args &= typ "DEM-input-file"
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
