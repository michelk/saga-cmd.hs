module Math.Geometry.Saga.Utils where
import Math.Geometry.Saga.Types
import qualified Data.Map  as M
import System.FilePath.Posix (dropExtension, replaceExtension)
import System.Directory (copyFile, renameFile)
import Text.Printf (printf)

defaultCmdPars :: SagaCmd -> [(String,String)]
defaultCmdPars (SagaCmd _ _ _ ps _ _ _ _) =
    map (\(k,(_,v)) -> (k,v)) (M.toList ps)

-- | Copy a grid data-set
copyGrid :: FilePath -> FilePath -> IO ()
copyGrid f t = mapM_ cp ["sgrd", "sdat", "mgrd"]
   where
     cp ext = copyFile (replaceExtension f ext) (replaceExtension t ext)

-- | Move a grid data-set
moveGrid :: FilePath -> FilePath -> IO ()
moveGrid f t = mapM_ cp ["sgrd", "sdat", "mgrd"]
   where
     cp ext = renameFile (replaceExtension f ext) (replaceExtension t ext)

-- | Utility function to append to basename of a file-name
appendFileName :: FilePath -> String -> FilePath
appendFileName f s = dropExtension f ++ s

-- | Dispatch on field seperator
-- dispSep :: String -> String
-- dispSep s = case s of
--     "space"     -> "space"
--     "Space"     -> "space"
--     " "         -> "space"
--     "\t"        -> "tabulator"
--     "tab"       -> "tabulator"
--     "Tab"       -> "tabulator"
--     "Tabulator" -> "tabulator"
--     ";"         -> ";"
--     ","         -> ","

-- Render a list of string-tuple in two columns
renderStringPairs :: [(String, String)] -> String
renderStringPairs = concatMap renderPair
  where
    renderPair :: (String, String) -> String
    renderPair (k,v) = twoCol k v

-- | render two strings in two columns
twoCol :: String -> String -> String
twoCol = printf "\t%10s  %10s\n"

writeReclassifyTableFlatSlope :: String -> IO ()
writeReclassifyTableFlatSlope f = putStrLn $ unlines [
  "minimum\tmaximum\tnew"
  ,"00.00000\00.10000\1.000000"
  ,"00.10000\00.20000\2.000000"
  ,"00.20000\00.50000\3.000000"
  ,"00.50000\01.00000\4.000000"
  ,"01.00000\05.00000\5.000000"
  ,"05.00000\25.00000\6.000000"
  ,"25.00000\50.00000\7.000000"
  ]
