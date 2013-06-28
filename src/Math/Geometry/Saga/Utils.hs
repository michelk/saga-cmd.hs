module Math.Geometry.Saga.Utils where
import System.FilePath.Posix (dropExtension, replaceExtension)
import System.Directory (copyFile)

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
