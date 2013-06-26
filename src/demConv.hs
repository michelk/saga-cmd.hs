{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import System.Console.CmdArgs
import Math.Geometry.Saga.Cmd
import Control.Monad (when)
import Data.Text (split, pack, unpack, Text)
import qualified Data.HashMap.Lazy as M
import Data.Maybe (fromJust)
import Text.Printf (printf)
import System.Environment (getArgs, withArgs)

_PROGRAM_NAME    = "demConv"
_PROGRAM_VERSION = "0.0.0.1"
_PROGRAM_INFO    = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_COPYRIGHT       = "GPL licensed; written by Michel Kuhlmann 2013"
_PROGRAM_ABOUT   = "Convert Digital Elevation Models (DEM) to diffent formats"
_PROGRAM_DETAILS = lines ( "Possible from-to-combinations:\n"
                   ++ (renderFromToKeys $ createConvDB defaultParams)
                   ++ "\n\n"
                   ++ "Default parameters:\n"
                   ++ (renderStringPairs $ M.toList defaultParams))

main :: IO ()
main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) (cmdArgs defaultOpts)
    let cmdPars = parseParamCmdString $ parameters opts
        pars    = adjustDefaultParams (cmdPars) defaultParams
        convDB  = createConvDB pars
        convFun = case M.lookup (from opts, to opts) convDB of
            Just f -> f
            Nothing -> error $ "from-to-combination not supported."
                       ++ "Currently supported: \n"
                       ++ (renderFromToKeys convDB)
    result <- convFun (file opts)
    putStrLn ("Created file " ++ result)

-- | Default conversion-parameters
defaultParams :: Params
defaultParams = M.fromList [
    ("xyzSep"      , "space")
   ,("xyzCellSize" , "1")
   ,("contourMin"  , "0")
   ,("contourMax"  , "10000")
   ,("contourStep" , "1")
   ,("tinMethod"   ,  "Opposite Neighbours")
   ]

-- | Convsersion data-base
createConvDB :: Params -> ConvDB
createConvDB p = M.fromList [
    (("xyz-grid"    , "grid"        ), xyzGridToGrid cs sep)
   ,(("xyz-grid"    , "grid-filled" ), xyzGridToFilledGrid)
   ,(("xyz-grid"    , "hillshade"   ), xyzGridToHillShade)
   ,(("grid"        , "hillshade"   ), gridToHillShade )
   ,(("grid-filled" , "hillshade"   ), gridHillshade)
   ,(("xyz-grid"    , "contour"     ), xyzGridToContour)
   ,(("grid"        , "contour"     ), gridToContour)
   ,(("grid-filled" , "contour"     ), gridContour cntrMin cntrMin cntrStep)
   ,(("xyz-grid"    , "tif"         ), xyzGridToTif)
   ,(("grid"        , "tif"         ), gridToTif)
   ,(("grid-filled" , "tif"         ), gridTif)
    ]
  where
    sep                   = lkp p "xyzSep"
    cs,cntrStep,cntrMin,cntrMax :: Double
    (cs:cntrStep:cntrMin:cntrMax:[]) = 
      map (read . lkp p) ["xyzCellSize","contourStep", "contourMin", "contourMax"]
    xyzGridToFilledGrid f = xyzToGrid cs sep f >>= gridFillGaps
    xyzGridToHillShade f  = xyzGridToFilledGrid f >>= gridHillshade
    gridToHillShade f     = gridFillGaps f >>= gridHillshade
    xyzGridToContour f    = 
      xyzGridToFilledGrid f >>= gridContour cntrMin cntrMin cntrStep
    gridToContour f       = gridFillGaps f >>= gridContour cntrMin cntrMin cntrStep
    xyzGridToTif f        = xyzGridToFilledGrid f >>= gridTif
    gridToTif f           = gridFillGaps f >>= gridTif

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
type ConvDB = M.HashMap (String,String) (String -> IO String)

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
    summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT) &=
    details _PROGRAM_DETAILS


-- | lookup a certain key in  in 'Params'
lkp :: Params -> String ->  String
lkp p' s = fromJust $ M.lookup s p'

-- | Parse the command-line string specifying parameters
parseParamCmdString :: String -> Params
parseParamCmdString = M.fromList . map parseAssign . splitStr ':'
  where
    parseAssign :: String -> (String,String)
    parseAssign s = let k:v:[] = splitStr '=' s in (k,v)

-- | Overwrite the default Parameters
adjustDefaultParams ::
       Params                   -- ^ parameters given on the command-line
    -> Params                   -- ^ default parameters
    -> Params                   -- ^ adjusted paramters
adjustDefaultParams pCmd pDef = case any (==False) validParas of
    True ->
        error $ "Invalid parameters on the command-line specified. Valid are \n" ++
         (unwords (M.keys pDef))
    _    -> M.union pDef pCmd
  where
    validParas :: [Bool]
    validParas = map  (\x -> M.member x pDef) (M.keys pCmd)


-- | Split a String on a certain delimiter
splitStr :: Char -> String -> [String]
splitStr c s = map unpack $ split (== c) (pack s)

-- | Render the keys of a 'ConvDB'
renderFromToKeys :: ConvDB -> String
renderFromToKeys db = twoCol "from" "to" ++ renderStringPairs (M.keys db)

-- Render a list of string-tuple in two columns
renderStringPairs :: [(String, String)] -> String
renderStringPairs ps = concatMap renderPair ps
  where
    renderPair :: (String, String) -> String
    renderPair (k,v) = twoCol k v
    
-- | render two strings in two columns
twoCol :: String -> String -> String
twoCol a b = printf "\t%10s  %10s\n" a b 
