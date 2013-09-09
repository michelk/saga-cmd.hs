module Math.Geometry.Saga.Doc where
import qualified Data.Map as M
import           Math.Geometry.Saga.Types

class TableView a where renderTable :: a -> String
instance TableView SagaIoCmdDB where
  renderTable db = unlines $ map renderTable $ M.toList db

instance TableView (String, SagaIoCmdExt) where
  let SagaCmd sLib sMod _ sParas _ _ _ _ = cmd "" "" in
    renderTable (cmdName, (cmd,ext)) = unlines [
      cmdName ++ " (" ++ sLib ++ " "  ++ sMod ++ ")"
      ,"Default suffix: " ++ ext
      ,renderTable  sParas
      ]

instance TableView ParaMap where
  renderTable = unlines . map renderTable . M.toList


instance TableView [(String, (String,String))] where
  renderTable (cmdArg, (sArg,def)) =
    cmdArg ++ "(" ++ "default:" ++ def ++ "; SAGA-Arg: " ++ sArg ++ ")"
