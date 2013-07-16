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
adjustSagaCmdParas :: SagaCmd -> CmdPars -> SagaCmd
adjustSagaCmdParas (SagaCmd lib mod ks prs pre post fIn fOut) pars = 
  SagaCmd lib mod ks prs' pre post fIn fOut
  where
    prs' = adjustParas prs pars
  
-- | Overwrite default parameters with parameters given on the command-line
adjustParas :: ParaMap          -- ^ parameters specified in 'SagaCmd'
               -> CmdPars       -- ^ parameters given on the cmd-line
               -> ParaMap       -- ^ adjusted parameters
adjustParas sPars cmdPars = foldr update' sPars (M.toList cmdPars')
  where
    cKeys = M.keys cmdPars
    sKeys = M.keys sPars
    cKeys' =                    -- ^ filter cmd-keys not relevant
      filter (`elem` sKeys) cKeys 
    cmdPars' =                  -- ^ only relevant parameters kept
      foldr M.delete cmdPars cKeys' 
    update' (k,v) = M.adjust (\(name,_) -> (name,v)) k

-- | Execute a 'CmdChain'
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
     convert ioCmd fIn = (\fIn -> doSaga (ioCmd fIn))
