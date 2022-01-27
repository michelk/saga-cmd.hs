module Gis.Saga.Cmd where

import Data.Map (elems)
import qualified Data.Map as M
import Data.Maybe
  ( fromJust,
    fromMaybe,
  )
import GHC.IO.Exception
import Gis.Saga.Types
import Gis.Saga.Utils
import System.Directory (getTemporaryDirectory)
import System.FilePath.Posix
  ( joinPath,
    replaceDirectory,
  )
import System.Posix.Temp (mkdtemp)
import System.Process (system)

-- | Actual Program to do the work
progName :: String
progName = "saga_cmd"

-- | Call saga with a specific configuration
doSaga :: Bool -> SagaCmd -> IO FilePath
doSaga ignoreExit (SagaCmd lib mdl (kIn, kOut) ps maybePre maybePost fOut fIn) =
  do
    pre fIn fOut
    r <- saga lib mdl (elems ps ++ [(kIn, fIn), (kOut, fOut)])
    case (r, ignoreExit) of
      (ExitSuccess, _) -> do
        post fIn fOut
        return fOut
      (ExitFailure _, True) -> return fOut
      (ExitFailure err, _) -> error ("saga_cmd failed: " <> show err)
  where
    pre = fromMaybe nthn maybePre
    post = fromMaybe nthn maybePost
    nthn _ _ = return ()

showSaga :: SagaCmd -> String
showSaga (SagaCmd lib mdl (kIn, kOut) ps _ _ fOut fIn) =
  sagaCmd lib mdl (elems ps ++ [(kIn, fIn), (kOut, fOut)])

-- | Wrapper around saga
saga ::
  -- | Library name
  String ->
  -- | Module name
  String ->
  -- | Parameter key-value
  [(String, String)] ->
  -- | Output-file
  IO ExitCode
saga lib mod params = do
  putStrLn cmd
  system cmd
  where
    cmd = sagaCmd lib mod params

sagaCmd ::
  -- | Library name
  String ->
  -- | Module name
  String ->
  -- | Parameter key-value
  [(String, String)] ->
  -- | Output-file
  String
sagaCmd lib mod params = cmd
  where
    renderParams = map renderPara
    renderPara (k, v) = "-" ++ k ++ "=" ++ v
    cmd = unwords [progName, lib, mod, unwords . renderParams $ params]

-- | adjust default parameters with the ones given on the command-line
adjustSagaCmdParas :: CmdPars -> SagaCmd -> SagaCmd
adjustSagaCmdParas cmdPrs cmd@SagaCmd {sParas = libPrs} =
  cmd {sParas = adjustParas libPrs cmdPrs}

-- | Overwrite default parameters with parameters given on the command-line
adjustParas ::
  -- | parameters specified in 'SagaCmd'
  ParaMap ->
  -- | parameters given on the cmd-line
  CmdPars ->
  -- | adjusted parameters
  ParaMap
adjustParas libPrs cmdPrs = M.mapWithKey lkp m'
  where
    m = M.union cmdPrs $ M.map snd libPrs

    m' = M.filterWithKey (\k _ -> k `elem` M.keys libPrs) m
    lkp k v = (fst $ fromJust (M.lookup k libPrs), v)

-- | Execute a 'SagaIoCmdExt'
doCmdChain ::
  Bool ->
  [SagaIoCmdExt] ->
  CmdPars ->
  FilePath ->
  Maybe FilePath ->
  IO FilePath
doCmdChain ignoreExit chain pars fIn fOut = do
  let outFsDefault = tail $ scanl appendFileName fIn (map snd chain)
  outFs <- case fOut of
    Nothing -> return outFsDefault
    Just f -> do
      dtempDir <- getTemporaryDirectory
      let dtempT = joinPath [dtempDir, "sagaPipe"]
      dtemp <- mkdtemp dtempT
      return $ map (`replaceDirectory` dtemp) (init outFsDefault) ++ [f]
  let chain' = zipWith (\f ext -> f ext) (map fst chain) outFs
  foldl
    ( \fOut' f -> do
        fIn' <- fOut'
        doSaga ignoreExit . adjustSagaCmdParas pars . f $ fIn'
    )
    (return fIn)
    chain'

-- | Show a 'SagaIoCmdExt'
showCmdChain ::
  [SagaIoCmdExt] -> CmdPars -> FilePath -> Maybe FilePath -> [String]
showCmdChain chain pars fIn _ =
  scanl
    (\fOut' f -> showSaga . adjustSagaCmdParas pars . f $ fOut')
    fIn
    chain'
  where
    outFsDefault = tail $ scanl appendFileName fIn (map snd chain)
    outFs = outFsDefault
    chain' = map (\(f, ext) -> f ext) $ zip (map fst chain) outFs

-- | Lookup a chain
lkpChain :: SagaIoCmdDB -> [String] -> [SagaIoCmdExt]
lkpChain db = map (`lkpCmd` db)

-- | Lookup a single command
lkpCmd :: String -> SagaIoCmdDB -> SagaIoCmdExt
lkpCmd s db =
  fromMaybe (error "Command is not yet implemented") $ M.lookup s db
