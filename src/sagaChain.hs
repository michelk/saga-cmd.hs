{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import System.Console.CmdArgs
import Math.Geometry.Saga.Types
import Math.Geometry.Saga.Chain
import Math.Geometry.Saga.Data
import Control.Monad (when)
import Data.Text (split, pack, unpack, Text)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Text.Printf (printf)
import System.Environment (getArgs, withArgs)

_PROGRAM_NAME    = "demConv"
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
        cmdPars :: CmdPars
        chain :: [String]
        chain = fromMaybe (error "from-to-combination not supported")
                          (M.lookup (from opts, to opts) sChainDB)
    result <- doCmdChain sCmdDB chain cmdPars (file opts)
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
    , parameters  = def &= help "Parameters to pass into the different conversion steps, delimited by ':'(eg cs=0.5:d=0.5)"
    , file        = def &= args &= typ "DEM-input-file"
    } &=
    program _PROGRAM_NAME &=
    help _PROGRAM_ABOUT &=
    summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT) 

-- | Parse the command-line string specifying parameters
parseParamCmdString :: String -> CmdPars
parseParamCmdString = M.fromList . map parseAssign . splitStr ':'
  where
    parseAssign :: String -> (String,String)
    parseAssign s = let k:v:[] = splitStr '=' s in (k,v)

-- | Overwrite the default Parameters
adjustDefaultParams ::
       CmdPars               -- ^ parameters given on the command-line
    -> CmdPars               -- ^ default parameters
    -> CmdPars               -- ^ adjusted paramters
adjustDefaultParams pCmd pDef = if False `elem` validParas 
    then
        error $ "Invalid parameters on the command-line specified. Valid are \n" ++
         unwords (M.keys pDef)
    else pCmd `M.union` pDef 
  where
    validParas :: [Bool]
    validParas = map  (`M.member` pDef) (M.keys pCmd)

-- | Split a String on a certain delimiter
splitStr :: Char -> String -> [String]
splitStr c s = map unpack $ split (== c) (pack s)

-- | Render the keys of a 'ConvDB'
--renderFromToKeys :: ConvDB -> String
renderFromToKeys db = twoCol "from" "to" ++ renderStringPairs (M.keys db)

-- Render a list of string-tuple in two columns
renderStringPairs :: [(String, String)] -> String
renderStringPairs = concatMap renderPair 
  where
    renderPair :: (String, String) -> String
    renderPair (k,v) = twoCol k v
    
-- | render two strings in two columns
twoCol :: String -> String -> String
twoCol = printf "\t%10s  %10s\n"