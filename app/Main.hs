module Main where

import System.Environment
import Numeric.Natural
import System.Exit
import Data.Maybe
import Data.List
import Data.Char
import Text.Read

main :: IO ()
main = getArgs >>= parse >>= putStrLn

-- PARSING ARGS -------------------------------------

parse :: [String] -> IO (String)
parse [] = usage >> exitError
parse [n, e] = usage >> exitError
parse [n, e, path]
    | isNothing checkN = usage >> exitError
    | isNothing checkE = usage >> exitError
    | otherwise = getFile $ path
    where
        checkN = readMaybe n :: Maybe Int
        checkE =  readMaybe e :: Maybe Double
parse otherwise = usage >> exitError

-- READ FILE ----------------------------------------

getFile :: String -> IO (String)
getFile path = readFile path

-- TOOLS --------------------------------------------

usage :: IO ()
usage = putStrLn "USAGE: ./imageCompressor n e IN\n\n\tn\tnumber of colors in the final image\n\te\tconvergence limit\n\tIN\tpath to the file containing the colors of the pixels"

exitError :: IO a
exitError = exitWith (ExitFailure 84)

exit :: IO Int
exit = exitWith ExitSuccess

