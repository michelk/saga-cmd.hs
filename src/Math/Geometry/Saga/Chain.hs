module Math.Geometry.Saga.Chain where
import Math.Geometry.Saga.Types
import Math.Geometry.Saga.Cmd
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import qualified Data.Map as M

-- | Execute a 'CmdChain'
doCmdChain ::    CmdDB          -- ^ Module data-base
              -> CmdChain       -- ^ sequence of commands with parameters
              -> FilePath       -- ^ Input-file
              -> IO FilePath    -- ^ Output-file
doCmdChain db chain fIn = undefined -- foldM >=> f convFuns

-- | Lookup a conversion function based on a module-name and parameters
lkpFunDB :: CmdDB                      -- ^ module-data-base
            -> String                  -- ^ module-name
            -> [(String,String)]       -- ^ parameters
            -> (FilePath -> IO String) -- ^ Conversion-function
lkpFunDB db k pars = undefined
  where
    SagaCmd lib mod ext ks prs pre post = fromMaybe
          (error $ k ++ " : Conversion function not supported")
          (M.lookup k db)
    --cmd = SagaCmd lib mod 

