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

