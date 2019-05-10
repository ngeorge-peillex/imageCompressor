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
import Numeric

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
    let pixel = (getPixelTab file)
    let cluster =  (pixelsToColors(getCluster pixel [] nbr g))
    let index = (getClustersIndexs pixel (cluster))
    clusterLoop nbr pixel index cluster 

-- CLUSTERISATION -----------------------------------


clusterLoop :: Int -> [Pixel] -> [Int] -> [(Double, Double, Double)] -> IO()
clusterLoop nbr pixel index cluster = do
    let newCluster = adjustClusters (getClusterTab pixel index nbr) nbr 0 cluster []
    let newIndex = (getClustersIndexs pixel (newCluster))
    if ((cmpList index newIndex) == False)
        then clusterLoop nbr pixel newIndex newCluster
        else do displayFinal cluster $ (getClusterTab pixel index nbr)
                --print $ (cluster) 
                --print $ (getClusterTab pixel index nbr)


adjustClusters :: [[Pixel]] -> Int -> Int -> [(Double, Double, Double)] -> [(Double, Double, Double)] -> [(Double, Double, Double)]
adjustClusters clusterTab k it clusters newClusters = do
    let tab = (pixelsToColors (clusterTab!!it))
    if (it < k)
        then
            adjustCluster clusterTab (colorsToUnique tab 1) (colorsToUnique tab 2) (colorsToUnique tab 3) k it clusters newClusters
        else
            newClusters


adjustCluster :: [[Pixel]] -> [Double] -> [Double] -> [Double] -> Int -> Int -> [(Double, Double, Double)] -> [(Double, Double, Double)] -> [(Double, Double, Double)]
adjustCluster clusterTab ra ga ba k it clusters newClusters = do
    let r = (sum ra) / (fromIntegral (length ra))
    let g = (sum ga) / (fromIntegral (length ga))
    let b = (sum ba) / (fromIntegral (length ba))
    adjustClusters clusterTab k (it + 1) clusters  (newClusters ++ [(r, g, b)])



getClusterTab :: [Pixel] -> [Int] -> Int -> [[Pixel]]
getClusterTab pixel index nbr = [[ pixel!!y | y <- [0..((length index) -1)], x == (index!!y)] | x <- [0..(nbr - 1)]]


getPixelTab ::String -> [Pixel]
getPixelTab file = catMaybes ( map (readPixel) (lines file))

averageColor :: [Pixel] -> Color
averageColor array = Color 1 2 3

getCluster :: [Pixel] -> [Pixel] -> Int -> StdGen -> [Pixel]
getCluster array list k g
    | k <= 0 = list
    | otherwise = getCluster array (list ++ [array!!(rdm)]) (k-1) g2
    where (rdm, g2) = randomR (0, length array -1) g

getPointDistance :: Double -> Double -> Double -> Double -> Double -> Double -> Double
getPointDistance xa xb ya yb za zb = sqrt((xb-xa)^2 + (yb-ya)^2 + (zb-za)^2)

getClustersIndexs :: [Pixel] -> [(Double, Double, Double)] -> [Int]
getClustersIndexs pixel cluster = (map (getClusterIndex 0 (-1) (0-1)  cluster) pixel)


getClusterIndex :: Int -> Double -> Int -> [(Double, Double, Double)] -> Pixel -> Int
getClusterIndex it dmin min cluster pixel
    | (length cluster > it) = computeClusterIndex it dmin min pixel pixel cluster
    | otherwise = min

computeClusterIndex :: Int -> Double ->  Int -> Pixel -> Pixel -> [(Double, Double, Double)] -> Int
computeClusterIndex it dmin min pixel (Pixel point (Color ra ga ba)) cluster
    | (min < 0 || (getPointDistance rb (fromIntegral ra) gb (fromIntegral ga) bb (fromIntegral ba)) < (dmin)) =
        getClusterIndex (it + 1) (getPointDistance rb (fromIntegral ra) gb (fromIntegral ga) bb (fromIntegral ba)) (it) cluster pixel
    | otherwise = getClusterIndex (it + 1) dmin min cluster pixel
    where (rb, gb, bb) = cluster!!it

-- DISPLAY ------------------------------------------

displayAverage :: (Double, Double, Double) -> IO ()
displayAverage (r, g, b) = putStrLn ("--\n(" ++ (showFFloat (Just 2) r "") ++ "," ++ (showFFloat (Just 2) g "") ++ "," ++ (showFFloat (Just 2) b "") ++ ")\n-")

displayCluster :: [Pixel] -> IO [()]
displayCluster array = mapM (putStrLn.show) array

displayAll :: ((Double, Double, Double), [Pixel]) -> IO ()
displayAll ((r, g, b), pixel) = do
    displayAverage $ (r, g, b)
    --print $ pixel
    displayCluster $ pixel
    return ()
    --putStrLn "Final display\n"

displayFinal :: [(Double, Double, Double)] -> [[Pixel]] -> IO ()
displayFinal averages array = mapM_ displayAll (zip averages array)

-- TOOLS --------------------------------------------

cmpList ::  [Int] -> [Int] -> Bool
cmpList a b = sort a == sort b 

pixelToColor :: Pixel -> (Double, Double, Double)
pixelToColor (Pixel point (Color r g b)) = (fromIntegral r, fromIntegral g, fromIntegral b)

pixelsToColors :: [Pixel] -> [(Double, Double, Double)]
pixelsToColors array = map pixelToColor array 

colorToUnique ::  Int -> (Double, Double, Double) -> Double
colorToUnique nbr (r, g, b) 
    | nbr == 1 = r
    | nbr == 2 = g
    | otherwise = b

colorsToUnique :: [(Double, Double, Double)] -> Int -> [Double]
colorsToUnique array nbr = map (colorToUnique nbr) (array)

usage :: IO ()
usage = putStrLn "USAGE: ./imageCompressor n e IN\n\n\tn\tnumber of colors in the final image\n\te\tconvergence limit\n\tIN\tpath to the file containing the colors of the pixels"

exitError :: IO a
exitError = exitWith (ExitFailure 84)

exit :: IO Int
exit = exitWith ExitSuccess
