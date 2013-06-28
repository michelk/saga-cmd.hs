module Math.Geometry.Saga.Types where
import qualified Data.Map as M

-- | Information needed to call saga_cmd
data SagaCmd = SagaCmd {
     sLib     :: String          -- ^ library to call
    ,sMod     :: String          -- ^ module number
    ,sOutExt  :: String          -- ^ extension to add to input-file
    ,sInOutKey:: (String,String) -- ^ Keys to use for input and output
    ,sParas   :: ParaMap -- ^ Parameters in addition to input and output -- 
    ,sPre     :: FilePath -> IO () -- ^ Pre-processing with input-file
    ,sPost    :: FilePath -> IO () -- ^ Post-processing with output-file
    }

-- | Parameter-Map
type ParaMap = [(Maybe String -- ^ Parameter-name accessable from the cmd-line
               ,String        -- ^ Parameter-name called in saga_cmd
               ,String        -- ^ Default-value to use
               )]
-- | Data-base for implemeted saga-modules
type CmdDB = M.Map
             String             -- ^ name of module-call
             SagaCmd            -- ^ configuration of module-call
-- | A chain of saga_cmd calls, where each output-file gets piped as
--   input to follwing command
type CmdChain = [
    (String                     -- name of module-call
    ,[(String,String)])         -- Parameters associated to the module-call
    ]
