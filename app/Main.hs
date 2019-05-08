module Main where

import System.Environment
import System.Exit
import Data.String
import Data.Maybe
import Data.List
import Data.Char
import Data.Typeable
import System.Random
import Text.Read

data Point = Point Int Int
instance Show Point where
    show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"


data Color = Color Int Int Int
instance Show Color where
    show (Color r g b) = "(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

data Pixel = Pixel Point Color
instance Show Pixel where
    show (Pixel point color) = show point ++ " " ++ show color 
    
main :: IO ()
main = getArgs >>= parse

-- PARSING ARGS -------------------------------------

parse :: [String] -> IO ()
parse [] = usage >> exitError
parse [n, e, path]
    | isNothing checkN = usage >> exitError
    | isNothing checkE = usage >> exitError
    | fromJust checkN < 1 = usage >> exitError
    | otherwise = getFile (fromJust checkN) path
    where
        checkN = readMaybe n :: Maybe Int
        checkE = readMaybe e :: Maybe Double
parse otherwise = usage >> exitError

-- READ FILE ----------------------------------------

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
                    where (as,bs) = splitAt n xs

readPixel :: String -> Maybe Pixel
readPixel line = let index = elemIndex ')' line
                    in case index of
                        Just index -> tupleToPixel first second
                                        where tmp = splitAt (index + 1) line
                                              first = readMaybe $ fst tmp :: Maybe (Int, Int)
                                              second = readMaybe $ snd tmp :: Maybe (Int, Int, Int)
                        Nothing -> Nothing

tupleToPixel :: Maybe (Int, Int) -> Maybe (Int, Int, Int) -> Maybe Pixel
tupleToPixel Nothing _ = Nothing
tupleToPixel _ Nothing = Nothing
tupleToPixel (Just (x, y)) (Just (r, g, b))
    | x < 0 || y < 0 = Nothing
    | r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255 = Nothing
    | otherwise = Just $ Pixel (Point x y) (Color r g b)

getFile :: Int -> String -> IO ()
getFile nbr path = do
    file <- readFile path
    g <- newStdGen
    print $ getCluster  (catMaybes ( map (readPixel) (lines file))) [] nbr g
    print $ map (readPixel) (lines file)

-- CLUSTERISATION -----------------------------------

averageColor :: [Pixel] -> Color
averageColor array = Color 1 2 3


getCluster :: [Pixel] -> [Pixel] -> Int -> StdGen -> [Pixel]
getCluster array list k g = do
    let (rdm, g2) = randomR (0, length array - 1) g
    if (k <= 0)
        then
        list
        else
        getCluster array (list ++ [array!!(rdm)]) (k-1) g2




--getPointDistance :: Int -> Int -> Int -> Int -> Int
getPointDistance xa xb ya yb za zb = sqrt((xb-xa)^2 + (yb-ya)^2 + (zb-za)^2)



-- DISPLAY ------------------------------------------

displayAverage :: Color -> IO ()
displayAverage color = putStrLn ("--\n" ++ show color ++ "\n-\n")

displayCluster :: [Pixel] -> IO [()]
displayCluster array = mapM (putStrLn.show) array

displayAll :: [Pixel] -> IO ()
displayAll array = putStrLn "Final display\n"

-- TOOLS --------------------------------------------

usage :: IO ()
usage = putStrLn "USAGE: ./imageCompressor n e IN\n\n\tn\tnumber of colors in the final image\n\te\tconvergence limit\n\tIN\tpath to the file containing the colors of the pixels"

exitError :: IO a
exitError = exitWith (ExitFailure 84)

exit :: IO Int
exit = exitWith ExitSuccess
