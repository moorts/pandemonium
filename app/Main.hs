module Main (main) where

import Lib
import System.Environment
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    case args of
        [configPath, dataPath] -> pandemize configPath dataPath
        _ -> exitFailure
