#!/usr/bin/env runhaskell
module Main where
import           Data.List (intercalate)
import qualified Data.Map as M
import           Math.Geometry.Saga.Data

main :: IO ()
main = do
  putStrLn $ unlines [
    "digraph chains {"
    ,unlines . map chainToDotLine $ M.elems sChainDB
    ,"}"
    ]

chainToDotLine :: [String] -> String
chainToDotLine xs = "\t" ++ (intercalate " -> " . map quote $ xs) ++ ";"

quote :: String -> String
quote x = "\"" ++ x ++ "\""
