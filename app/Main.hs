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
    print $ (pixelsToColors(getCluster (getPixelTab file) [] nbr g))
    let pixel = (getPixelTab file)
    let index = (getClustersIndexs (getPixelTab file) (pixelsToColors(getCluster (getPixelTab file) [] nbr g)))
    print $  getClusterTab pixel index nbr

-- CLUSTERISATION -----------------------------------



adjustClusters :: [[Pixel]] -> Int -> Int -> [(Double, Double, Double)] -> [(Double, Double, Double)]
adjustClusters clusterTab k it clusters = do
    if (it < k)
        then
            adjustCluster clusterTab (pixelsToColors (clusterTab!!it)) k it clusters 
        else
            clusters


adjustCluster :: [[Pixel]] -> [(Double, Double, Double)] -> Int -> Int -> [(Double, Double, Double)] -> [(Double, Double, Double)]
adjustCluster clusterTab (ra ga ba) k it clusters = do
    let r = (sum ra) / (length ra) ::Double
    let g = (sum ga) / (length ga) ::Double
    let b = (sum ba) / (length ba) ::Double
    adjustClusters clusterTab k (it + 1) (clusters ++ [(r, g, b)])



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

displayAverage :: Color -> IO ()
displayAverage color = putStrLn ("--\n" ++ show color ++ "\n-\n")

displayCluster :: [Pixel] -> IO [()]
displayCluster array = mapM (putStrLn.show) array

displayAll :: [Pixel] -> IO ()
displayAll array = putStrLn "Final display\n"

-- TOOLS --------------------------------------------

pixelToColor :: Pixel -> (Double, Double, Double)
pixelToColor (Pixel point (Color r g b)) = (fromIntegral r, fromIntegral g, fromIntegral b)

pixelsToColors :: [Pixel] -> [(Double, Double, Double)]
pixelsToColors array = map pixelToColor array 

colorToR :: (Double, Double, Double) -> Int -> Double
colorToR (r, g, b) nbr
    | nbr == 1 = r
    | nbr == 2 = g
    | otherwise = b

colorsToUnique :: [(Double, Double, Double)] -> Int -> [Double]
colorsToUnique array nbr = map (colorToUnique) (nbr array)

usage :: IO ()
usage = putStrLn "USAGE: ./imageCompressor n e IN\n\n\tn\tnumber of colors in the final image\n\te\tconvergence limit\n\tIN\tpath to the file containing the colors of the pixels"

exitError :: IO a
exitError = exitWith (ExitFailure 84)

exit :: IO Int
exit = exitWith ExitSuccess
