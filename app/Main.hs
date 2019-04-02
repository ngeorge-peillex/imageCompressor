module Main where

import System.Environment
import System.Exit
import Data.String
import Data.Maybe
import Data.List
import Data.Char
import Data.Typeable
import Text.Read

main :: IO ()
main = getArgs >>= parse

-- PARSING ARGS -------------------------------------

parse :: [String] -> IO ()
parse [] = usage >> exitError
parse [n, e, path]
    | isNothing checkN = usage >> exitError
    | isNothing checkE = usage >> exitError
    | fromJust checkN < 1 = usage >> exitError
    | otherwise = getFile path
    where
        checkN = readMaybe n :: Maybe Int
        checkE = readMaybe e :: Maybe Double
parse otherwise = usage >> exitError

-- READ FILE ----------------------------------------

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = split cs delim

getFile :: String -> IO ()
getFile path = do
    file <- readFile path
    print $ lines $ file

-- TOOLS --------------------------------------------

usage :: IO ()
usage = putStrLn "USAGE: ./imageCompressor n e IN\n\n\tn\tnumber of colors in the final image\n\te\tconvergence limit\n\tIN\tpath to the file containing the colors of the pixels"

exitError :: IO a
exitError = exitWith (ExitFailure 84)

exit :: IO Int
exit = exitWith ExitSuccess

