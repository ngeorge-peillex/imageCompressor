module Main where

import System.Environment
import System.Exit
import Data.String
import Data.Maybe
import Data.List
import Data.Char
import Data.Typeable
import Text.Read

data Point = Point Int Int
            deriving Show
data Color = Color Int Int Int
            deriving Show
data Pixel = Pixel Point Color
            deriving Show

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

readPixel :: String -> Maybe Pixel
readPixel line = let tmp = splitAt 6 line
                     first = readMaybe $ fst tmp :: Maybe (Int, Int)
                     second = readMaybe $ snd tmp :: Maybe (Int, Int, Int)
                     in tupleToPixel first second

tupleToPixel :: Maybe (Int, Int) -> Maybe (Int, Int, Int) -> Maybe Pixel
tupleToPixel Nothing _ = Nothing
tupleToPixel _ Nothing = Nothing
tupleToPixel (Just (x, y)) (Just (r, g, b))
    | x < 0 || y < 0 = Nothing
    | r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255 = Nothing
    | otherwise = Just $ Pixel (Point x y) (Color r g b)

getFile :: String -> IO ()
getFile path = do
    file <- readFile path
    --print $ lines file !! 0
    print $ map (readPixel) (lines file)
    --print $ Pixel (Point 1 2) (Color 1 2 3)
    --print $ map (splitAt 6) (lines file)

-- TOOLS --------------------------------------------

usage :: IO ()
usage = putStrLn "USAGE: ./imageCompressor n e IN\n\n\tn\tnumber of colors in the final image\n\te\tconvergence limit\n\tIN\tpath to the file containing the colors of the pixels"

exitError :: IO a
exitError = exitWith (ExitFailure 84)

exit :: IO Int
exit = exitWith ExitSuccess
