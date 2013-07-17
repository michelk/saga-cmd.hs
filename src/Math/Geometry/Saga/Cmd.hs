module Math.Geometry.Saga.Cmd where
import           Data.Map (elems)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, fromJust)
import           GHC.IO.Exception
import           Math.Geometry.Saga.Types
import           Math.Geometry.Saga.Utils
import           Math.Geometry.Saga.Data
import           System.Cmd (system)

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
adjustSagaCmdParas cmdPrs (SagaCmd lib mod ks libPrs pre post fOut fIn) =
  SagaCmd lib mod ks prs' pre post fOut fIn
  where
    prs' = adjustParas libPrs cmdPrs

-- | Overwrite default parameters with parameters given on the command-line
adjustParas :: ParaMap          -- ^ parameters specified in 'SagaCmd'
               -> CmdPars       -- ^ parameters given on the cmd-line
               -> ParaMap       -- ^ adjusted parameters
adjustParas libPrs cmdPrs = M.mapWithKey lkp m
  where
    m  = M.union cmdPrs $ M.map snd libPrs
    lkp k v = (fst . fromJust $ M.lookup k libPrs, v)

-- | Execute a 'ChainSagaIoCmd'
doCmdChain :: [ChainSagaIoCmd] -> CmdPars -> FilePath -> IO FilePath
doCmdChain chain pars fIn =
  foldl (\fOut f -> do
            fIn' <- fOut
            fOut >>= convert f fIn') (return fIn) chain'
  where
     outExts = map snd chain
     cmds :: [SagaIoCmd]
     cmds = map fst chain
     outFs :: [String]
     outFs = scanl appendFileName fIn outExts
     chain' :: [FilePath -> SagaCmd]
     chain' = map (\(f,ext) -> f ext) $ zip  cmds outFs
     convert :: (FilePath -> SagaCmd) -> FilePath -> (FilePath -> IO FilePath)
     convert ioCmd fIn = (\fIn -> doSaga (adjustSagaCmdParas pars (ioCmd fIn)))
