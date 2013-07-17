module Main where
import System.Environment (getArgs, getProgName)
import Control.Monad (when)
import Math.Geometry.Saga.LUT

main :: IO ()
main = do
    args <- getArgs
    prg <- getProgName
    when (length args /= 2) (fail (usage prg))
    let k:j:[] = args
    putStr $ bgrColTable (read k) (read j)


usage :: String -> String
usage p = "Usage: " ++ p ++ " min max"
