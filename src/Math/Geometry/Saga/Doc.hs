module Math.Geometry.Saga.Doc where
import qualified Data.Map as M
import           Math.Geometry.Saga.Types

renderSagaIoDb :: SagaIoCmdDB -> String
renderSagaIoDb db = unlines $ map renderSagaIoCmd $ M.toList db

renderSagaIoCmd :: (String, SagaIoCmdExt) -> String
renderSagaIoCmd (cmdName, (cmd,ext)) = unlines [
  cmdName ++ " (" ++ sLib ++ " "  ++ sMod ++ ")"
 ,"Default suffix: " ++ ext
 ,renderParas $ M.toList sParas
 ]
  where
    SagaCmd sLib sMod _ sParas _ _ _ _ = cmd "" ""
    renderParas = unlines . map renderPara
    renderPara (cmdArg, (sArg,def)) =
      cmdArg ++ "(" ++ "default:" ++ def ++ "; SAGA-Arg: " ++ sArg ++ ")"
