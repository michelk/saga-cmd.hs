{-# LANGUAGE FlexibleInstances #-}
module Math.Geometry.Saga.Doc (renderTable, renderDot, renderNodes)
where
import qualified Data.Map as M
import           Math.Geometry.Saga.Types
import           Data.List (intercalate)
import           Text.Printf (printf)

class TableView a where renderTable :: a -> String

instance TableView SagaIoCmdDB where
  renderTable db =
   "Command (cmdPar,sagaPar,default) sagaLib sagaModule defaultSuffix\n" ++
    (unlines . map renderTable . M.toList $ db)

instance TableView (String, SagaIoCmdExt) where
  renderTable = renderTableSagaIoCmd

renderTableSagaIoCmd :: (String, SagaIoCmdExt) -> String
renderTableSagaIoCmd (cmdName, (cmd,ext)) =
  let SagaCmd sLib sMod _ sParas _ _ _ _ = cmd "" ""
  in unwords [cmdName, renderTable sParas, sLib, sMod, ext]

instance TableView ParaMap where
  renderTable pm
                 | M.size pm == 0  = "NA"
                 | otherwise = intercalate ":" (map renderTable . M.toList $ pm)

instance TableView (String, (String,String)) where
  renderTable (cmdArg, (sArg,def)) =
    "(" ++ intercalate "," [cmdArg,sArg,def] ++ ")"



class DotGraphics a where renderDot :: a -> String

instance DotGraphics (SagaIoCmdDB,NodeMap) where
  renderDot (cmds,chains) = unlines [
    "digraph chains {"
   ,"  graph  [rankdir = LR];"
   ,"  node [shape = ellipse, fontsize = 8];"
   ,""
   ,unlines . map renderDot . M.toList $ cmds -- implemented modules
   ,renderDot chains                            -- implemented chains
   ,"}"
   ]

instance DotGraphics (String, SagaIoCmdExt) where
  renderDot = renderDotSagaIoCmd

renderDotSagaIoCmd :: (String, SagaIoCmdExt) -> String
renderDotSagaIoCmd (cmdName, (cmd,ext)) =
  let SagaCmd sLib sMod _ sParas _ _ _ _ = cmd "" ""
  in printf "  %s [shape = record, label = \"%s|%s|%s|%s %s\"];"
     cmdName cmdName (renderDot sParas) ext  sLib  sMod


renderDotParaMap :: ParaMap -> String
renderDotParaMap pm = "{" ++ ss ++ "}"
  where
    ps = M.toList pm
    cmdArgs = intercalate "\\n" (map fst ps)
    sArgs = intercalate "\\n" (map (fst . snd)  ps)
    defs = intercalate "\\n" (map (snd . snd)  ps)
    ss = intercalate "|" [cmdArgs,sArgs,defs]

instance DotGraphics ParaMap where
  renderDot = renderDotParaMap

instance DotGraphics NodeMap where
  renderDot = unlines . map renderDot . M.toList

instance DotGraphics (String, ([String],[String])) where
  renderDot (name, (ins, outs)) = unlines $ map unlines [
    map (`edge` name) ins
   ,map (name `edge`) outs
   ]

edge :: String -> String -> String
edge = printf "  \"%s\" -> \"%s\";"

class NodeView a where renderNodes :: a -> String

instance NodeView NodeMap where
  renderNodes = unlines . map renderNodes . M.toList

instance NodeView (String, ([String], [String])) where
  renderNodes (name, (ins, outs)) = name ++ ": "++ show ins ++ show outs
