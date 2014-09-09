module Gis.Saga.Types where
import qualified Data.Map as M

-- | Parameters passed in with command-line
type CmdPars = M.Map String String

-- | Information needed to call saga_cmd
data SagaCmd = SagaCmd {
     sLib     :: String          -- ^ library to call
    ,sMod     :: String          -- ^ module number
    ,sInOutKey:: (String,String) -- ^ Keys to use for input and output
    ,sParas   :: ParaMap -- ^ Parameters in addition to input and output
    ,sPre     ::  Maybe (FilePath -> FilePath -> IO ()) -- ^ Pre-processing (input/output-file)
    ,sPost    :: Maybe (FilePath -> FilePath -> IO ()) -- ^ Post-processing (input/output-file)
    ,sOutFile :: FilePath          -- ^ Output-file
    ,sInFile  :: FilePath          -- ^ Input-file
    }

-- | Parameter-Map in a 'SagaCmd'
type ParaMap = M.Map String  (String,String)

-- | Saga Command which takes only one input-, and one output-file
type SagaIoCmd = FilePath -> FilePath -> SagaCmd

-- | SagaIO-Command with output file extension
type SagaIoCmdExt = (SagaIoCmd, String)

-- | Data-base with available Saga-Input-Output-commands
type SagaIoCmdDB = M.Map String SagaIoCmdExt

-- | Node-map for form-to-combinations
type NodeMap = M.Map String ([String],[String])
