module Math.Geometry.Saga.Chain (doCmdChain) where 
import Math.Geometry.Saga.Types
import Math.Geometry.Saga.Cmd
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import qualified Data.Map as M


-- | Execute a 'CmdChain'
doCmdChain ::    CmdDB          -- ^ Module data-base
              -> [String]       -- ^ sequence of module-calls
              -> CmdPars        -- ^ command-line parameters
              -> FilePath       -- ^ Input-file
              -> IO FilePath    -- ^ Output-file
doCmdChain db chain pars fIn = foldl (>>=) (return fIn) fs
  where
    fs :: [FilePath -> IO FilePath]
    fs = map (lkpFunDB db pars) chain

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

