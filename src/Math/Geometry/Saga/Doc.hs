module Math.Geometry.Saga.Doc where
-- import           Data.List (intercalate)
import qualified Data.Map as M
import           Math.Geometry.Saga.Data
import           Math.Geometry.Saga.Types

renderSagaIoDb :: SagaIoCmdDB -> String
renderSagaIoDb db = unlines $ map renderSagaIoCmd $ M.toList sIoDB

renderSagaIoCmd :: (String, SagaIoCmdExt) -> String
renderSagaIoCmd (cmdName, (cmd,ext)) = unlines [
  cmdName ++ " (" ++ sLib ++ " "  ++ sMod ++ ")"
 ,"Default suffix: " ++ ext
 ]
  where
    SagaCmd sLib sMod sInOutKey sParas sPre sPost sOutFile sInFile = cmd "" ""
    renderParas = undefined

-- renderIoCmd :: (String, SagaIoCmdExt) -> String
-- renderIoCmd (nme,((SagaCmd lib mod pars _ _ ), ext)) = undefined

-- chainToDotLine :: [String] -> String
-- chainToDotLine xs = "\t" ++ (intercalate " -> " . map quote $ xs) ++ ";"
--
-- quote :: String -> String
-- quote x = "\"" ++ x ++ "\""

--  putStrLn $ unlines [
--    "digraph chains {"
--    ,unlines . map chainToDotLine $ M.elems sChainDB
--    ,"}"
--    ]
