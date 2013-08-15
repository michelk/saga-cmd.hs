#!/usr/bin/env runhaskell
module Main where
import           Data.List (intercalate)
import qualified Data.Map as M
import           Math.Geometry.Saga.Data
import           Math.Geometry.Saga.Types

main :: IO ()
main = do
  putStrLn $ unlines [
    "digraph chains {"
    ,unlines . map chainToDotLine $ M.elems sChainDB
    ,"}"
    ]

renderIoCmd :: (String, SagaIoCmdExt) -> String
renderIoCmd (nme,((SagaCmd lib mod pars _ _ ), ext)) = undefined

chainToDotLine :: [String] -> String
chainToDotLine xs = "\t" ++ (intercalate " -> " . map quote $ xs) ++ ";"

quote :: String -> String
quote x = "\"" ++ x ++ "\""
