module Gis.Saga.Cmd where
import           Data.Map (elems)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, fromJust)
import           GHC.IO.Exception
import           Gis.Saga.Types
import           Gis.Saga.Utils
import           System.Process (system)
import           System.Posix.Temp (mkdtemp)
import           System.FilePath.Posix (replaceDirectory, joinPath)
import           System.Directory (getTemporaryDirectory)

-- | Actual Program to do the work
progName :: String
progName = "saga_cmd"

-- | Call saga with a specific configuration
doSaga :: SagaCmd -> IO FilePath
doSaga (SagaCmd lib mod (kIn, kOut) ps maybePre maybePost fOut fIn) = do
   pre fIn fOut
   r <- saga lib mod (elems ps ++ [(kIn,fIn),(kOut,fOut)])
   case r of
       ExitSuccess -> do
           post fIn fOut
           return fOut
       ExitFailure _ -> error "saga_cmd failed"
   where
     pre  = fromMaybe nthn maybePre
     post = fromMaybe nthn maybePost
     nthn _ _ =  return ()


-- | Wrapper around saga
saga ::
       String                   -- ^ Library name
    -> String                   -- ^ Module name
    -> [(String,String)]        -- ^ Parameter key-value
    -> IO ExitCode              -- ^ Output-file
saga lib mod params =
    system $ unwords [
        progName
       ,lib
       ,mod
       ,unwords . renderParams  $ params
       ]
  where
    renderParams = map renderPara
    renderPara (k,v) = "-" ++ k ++ " " ++ v


-- | adjust default parameters with the ones given on the command-line
adjustSagaCmdParas :: CmdPars ->  SagaCmd -> SagaCmd
adjustSagaCmdParas cmdPrs cmd@(SagaCmd{sParas = libPrs}) =
  cmd {sParas = adjustParas libPrs cmdPrs}

-- | Overwrite default parameters with parameters given on the command-line
adjustParas :: ParaMap          -- ^ parameters specified in 'SagaCmd'
               -> CmdPars       -- ^ parameters given on the cmd-line
               -> ParaMap       -- ^ adjusted parameters
adjustParas libPrs cmdPrs = M.mapWithKey lkp m'
  where
    -- | union of cmd-pars and lib-pars; this overwrites the defaults
    m  = M.union cmdPrs $ M.map snd libPrs
    -- | select relevant pars
    m' = M.filterWithKey (\k _ -> k `elem` M.keys libPrs) m
    lkp k v = (fst $ fromJust (M.lookup k libPrs), v)

-- | Execute a 'SagaIoCmdExt'
doCmdChain :: [SagaIoCmdExt] -> CmdPars -> FilePath -> Maybe FilePath -> IO FilePath
doCmdChain chain pars fIn fOut = do
  let outFsDefault = tail $ scanl appendFileName fIn (map snd chain)
  outFs <- case fOut of
    Nothing -> return outFsDefault
    Just f  -> do
      dtempDir <- getTemporaryDirectory
      let dtempT = joinPath [dtempDir, "sagaPipe"]
      dtemp <- mkdtemp dtempT
      return $ map (`replaceDirectory` dtemp) (init outFsDefault) ++ [f]
  let chain' = map (\(f,ext) -> f ext) $ zip (map fst chain) outFs
  foldl (\fOut f -> do
            fIn' <- fOut
            doSaga . adjustSagaCmdParas pars . f $ fIn'
        ) (return fIn) chain'

-- | Lookup a chain
lkpChain :: SagaIoCmdDB -> [String] -> [SagaIoCmdExt]
lkpChain db = map (`lkpCmd` db)

-- | Lookup a single command
lkpCmd :: String -> SagaIoCmdDB -> SagaIoCmdExt
lkpCmd  s db = fromMaybe (error "Command is not yet implemented") $ M.lookup s db


