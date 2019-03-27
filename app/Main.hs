module Main where

import System.Environment
import System.Exit
import Numeric.Natural
import Data.List
import Data.Char

main :: IO ()
main = getArgs >>= parse

-- PARSING ARGS -------------------------------------

parse [n, e, path] = do
    n <- checkNumber n
    e <- checkNumber e
    putStrLn "WORKS !!"

parse otherwise = do
    usage
    exitError

-- READ FILE ----------------------------------------



-- TOOLS --------------------------------------------

checkNumber :: String -> IO (Int)
checkNumber n = do
    if checkIsNumber n == True
        then return (read n::Int)
        else usage >> exitError

checkIsNumber :: String -> Bool
checkIsNumber n = case (reads n) :: [(Natural, String)] of
    [(_, "")] -> True
    _         -> False

usage :: IO ()
usage = putStrLn "USAGE: ./imageCompressor n e IN\n\n\tn\t\tnumber of colors in the final image\n\te\t\tconvergence limit\n\tIN\tpath to the file containing the colors of the pixels"
exitError = exitWith (ExitFailure 84)
exit = exitWith ExitSuccess

