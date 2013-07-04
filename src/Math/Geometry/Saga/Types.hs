module Math.Geometry.Saga.Types where
import qualified Data.Map as M

-- | Parameters passed in with command-line
type CmdPars = M.Map String String

-- | Information needed to call saga_cmd
data SagaCmd = SagaCmd {
     sLib     :: String          -- ^ library to call
    ,sMod     :: String          -- ^ module number
    ,sOutExt  :: String          -- ^ extension to add to input-file
    ,sInOutKey:: (String,String) -- ^ Keys to use for input and output
    ,sParas   :: ParaMap -- ^ Parameters in addition to input and output 
    ,sPre     :: FilePath -> IO () -- ^ Pre-processing with input-file
    ,sPost    :: FilePath -> IO () -- ^ Post-processing with output-file
    }

-- | Parameter-Map in a 'SagaCmd'
type ParaMap = 
  M.Map String          -- ^ Parameter-name accessor from the cmd-line
        (String,String) -- ^ Parameter-name called in saga_cmd( Default-value
               
-- | Data-base for implemeted saga-modules
type CmdDB = M.Map
             String             -- ^ name of module-call
             SagaCmd            -- ^ configuration of module-call

-- | Data-base for chains. Key contains from-to; value is a sequence
--   of module-calls
type ChainDB = M.Map (String, String) [String]
