module Math.Geometry.Saga.Cmd where
import           Data.Map (elems)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           GHC.IO.Exception
import           Math.Geometry.Saga.Types
import           Math.Geometry.Saga.Utils
import           System.Cmd (system)

-- | Actual Program to do the work
progName :: String
progName = "saga_cmd"

 -- | Call saga with a specific configuration
doSaga :: SagaCmd -> FilePath -> IO String
doSaga (SagaCmd lib mod extOut (kIn, kOut) ps pre post) fIn = do
   pre fIn
   r <- saga lib mod ps'
   case r of
       ExitSuccess -> do
           post outF
           return outF
       ExitFailure _ -> error "saga_cmd failed"
   where
     outF = appendFileName fIn extOut
     ps' = elems ps ++ [(kIn,fIn),(kOut,outF)]

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


-- | Lookup a conversion function based on a module-name and parameters
lkpFunDB ::    CmdDB                   -- ^ module-data-base
            -> CmdPars                 -- ^ parameters
            -> String                  -- ^ module-name
            -> (FilePath -> IO String) -- ^ Conversion-function
lkpFunDB db pars k = doSaga cmd
  where
    SagaCmd lib mod ext ks prs pre post = fromMaybe
          (error $ k ++ " : Conversion function not supported")
          (M.lookup k db)
    cmd = SagaCmd lib mod ext ks prs' pre post
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
      foldr (\x acc -> M.delete x acc) cmdPars cKeys' 
    update' (k,v) par = M.adjust (\(name,_) -> (name,v)) k par
