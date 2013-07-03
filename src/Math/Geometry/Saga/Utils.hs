module Math.Geometry.Saga.Utils where
import Math.Geometry.Saga.Types
import qualified Data.Map  as M
import System.FilePath.Posix (dropExtension, replaceExtension)
import System.Directory (copyFile)

-- defaultCmdPars :: SagaCmd -> CmdPars
-- defaultCmdPars (SagaCmd _ _ _ _ ps _ _) =
--     M.fromList $ map (\(pCmd,_,pDef) -> (pCmd,pDef)) ps 

-- | Copy a grid data-set
copyGrid :: FilePath -> FilePath -> IO ()
copyGrid f t = mapM_ cp ["sgrd", "sdat", "mgrd"]
   where
     cp ext = copyFile (replaceExtension f ext) (replaceExtension t ext)
    
-- | Utility function to append to basename of a file-name
appendFileName :: FilePath -> String -> FilePath
appendFileName f s = dropExtension f ++ s

-- | Dispatch on field seperator
dispSep :: String -> String
dispSep s = case s of
    "space"     -> "space"
    "Space"     -> "space"
    " "         -> "space"
    "\t"        -> "tabulator"
    "tab"       -> "tabulator"
    "Tab"       -> "tabulator"
    "Tabulator" -> "tabulator"
    ";"         -> ";"
    ","         -> ","
