{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad (when)
import qualified Data.Map as M
import Data.Maybe
  ( fromJust,
    fromMaybe,
  )
import Data.Text
  ( pack,
    split,
    unpack,
  )
import Gis.Saga.Cmd
import Gis.Saga.Data
import Gis.Saga.Doc
import Gis.Saga.Node (getAllRoutes)
import Gis.Saga.Types
import System.Console.CmdArgs
import System.Environment
  ( getArgs,
    withArgs,
  )
import System.Exit (exitSuccess)

main :: IO ()
main = do
  args <- getArgs
  -- If the user did not specify any arguments, pretend as "--help" was given
  opts <- (if null args then withArgs ["--help"] else id) (cmdArgs defaultOpts)
  when (modules opts) (sequence_ [putStrLn (renderTable sIoDB), exitSuccess])
  when
    (dot opts)
    (sequence_ [putStrLn (renderDot (sIoDB, sNodes)), exitSuccess])
  when (nodes opts) (sequence_ [putStrLn (renderNodes sNodes), exitSuccess])
  when (null $ file opts) (error "Please specify an input-file")
  let cmdPars = parseParamCmdString $ parameters opts
      allChains = M.fromList . getAllRoutes $ sNodes
      cmdChain = case chain opts of
        "" ->
          lkpChain
            sIoDB
            ( fromMaybe
                ( error
                    ( "from-to-combination not supported.\n\nSupported:\n "
                        ++ (unlines . map show . M.toList $ allChains)
                    )
                )
                (M.lookup (from opts, to opts) allChains)
            )
        _ -> lkpChain sIoDB (splitStr ':' $ chain opts)
  when
    (dryRun opts)
    ( sequence_
        [ putStrLn
            ( unlines $
                showCmdChain
                  cmdChain
                  cmdPars
                  (file opts)
                  ( case output opts of
                      "" -> Nothing
                      _ -> Just (output opts)
                  )
            ),
          exitSuccess
        ]
    )
  result <-
    doCmdChain
      (ignoreExit opts)
      cmdChain
      cmdPars
      (file opts)
      ( case output opts of
          "" -> Nothing
          _ -> Just (output opts)
      )
  putStrLn ("Succussfully created " ++ result)

_PROGRAM_NAME,
  _PROGRAM_VERSION,
  _PROGRAM_INFO,
  _PROGRAM_ABOUT,
  _COPYRIGHT ::
    String
_PROGRAM_NAME = "sagaPipe"
_PROGRAM_VERSION = "0.4"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_COPYRIGHT = "GPL licensed; written by Michel Kuhlmann 2020"
_PROGRAM_ABOUT = "Wrapper for saga_cmd"

-- | Data structure for command line options.
data Opt = Opt
  { from :: String,
    to :: String,
    parameters :: String,
    chain :: String,
    output :: FilePath,
    modules :: Bool,
    dot :: Bool,
    nodes :: Bool,
    dryRun :: Bool,
    ignoreExit :: Bool,
    file :: FilePath
  }
  deriving (Show, Data, Typeable)

-- | Defaults for command-line options.
defaultOpts :: Opt
defaultOpts =
  Opt
    { from = def &= help "Source-format",
      to = def &= help "Target-format",
      parameters =
        def
          &= help
            "Conversion-Parameters; delimited by ':'(eg cs=0.5:sep=tabulator)",
      chain =
        def
          &= help
            "Conversion-pathway; delimited by ':'(eg cXyzGridToGrid:cGridFillGaps)",
      output =
        def
          &= help "Output-file (optional; no intermediate files preserved)",
      modules = def &= help "Create a table of implemented modules",
      dot = def &= help "Show implemented chains as a dot-graphics",
      nodes = def &= help "Show implemented nodes",
      dryRun = def &= help "Only show commands instead of running them",
      ignoreExit = def &= help "Proceed also if command fails",
      file = def &= args &= typ "DEM-input-file"
    }
    &= program _PROGRAM_NAME
    &= help _PROGRAM_ABOUT
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)

-- | Parse the command-line string specifying parameters
parseParamCmdString :: String -> CmdPars
parseParamCmdString "" = M.empty
parseParamCmdString s = M.fromList . map parseAssign . splitStr ':' $ s
  where
    parseAssign :: String -> (String, String)
    parseAssign s = let k : v : [] = splitStr '=' s in (k, v)

-- | Split a String on a certain delimiter
splitStr :: Char -> String -> [String]
splitStr c s = map unpack $ split (== c) (pack s)
