--Fulop Koppany-Bence

import System.IO
import Control.Monad
import Data.List
import Data.Char

convertToInt :: [String] -> Int -> Int
convertToInt xs i = read (xs !! i) :: Int

okRows :: [[Int]] -> Bool
okRows xs = foldl (\acc x -> acc && ([1..9] == (r2t x))) True xs

okColumns :: [[Int]] -> Bool
okColumns xs = foldl (\acc x -> acc && ([1..9] == (r2t x))) True (transpose xs)

generateSquares1 :: [[Int]] -> [[Int]]
generateSquares1 [] = []
generateSquares1 (x:xs) = (take 3 x) : (generateSquares1 xs)

generateSquares2 :: [[Int]] -> [[Int]]
generateSquares2 [] = []
generateSquares2 (x:xs) = (drop 3 (take 6 x)) : (generateSquares2 xs)

generateSquares3 :: [[Int]] -> [[Int]]
generateSquares3 [] = []
generateSquares3 (x:xs) = (drop 6 (take 9 x)) : (generateSquares3 xs)

union1 :: [[Int]] -> [[Int]]
union1 xs = convert(take 3 (generateSquares1 xs)) : (convert(drop 3 (take 6 (generateSquares1 xs)))) : (convert(drop 6 (generateSquares1 xs))) : []

union2 :: [[Int]] -> [[Int]]
union2 xs = convert(take 3 (generateSquares2 xs)) : (convert(drop 3 (take 6 (generateSquares2 xs)))) : (convert(drop 6 (generateSquares2 xs))) : []

union3 :: [[Int]] -> [[Int]]
union3 xs = convert(take 3 (generateSquares2 xs)) : (convert(drop 3 (take 6 (generateSquares3 xs)))) : (convert(drop 6 (generateSquares3 xs))) : []

unionAll :: [[Int]] -> [[Int]]
unionAll xs = (union1 xs) ++ (union2 xs) ++ (union3 xs)

okSquares :: [[Int]] -> Bool
okSquares xs = foldl (\acc x -> acc && ([1..9] == (r2t x))) True (unionAll xs)

convert [] = []
convert (x:xs) = x ++ convert xs

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    r <- replicateM 9 $ do
        input_line <- getLine
        let input = words input_line

        r2 <- forM [0..(9-1)] $ \i -> do
            let n = convertToInt
         input i
            return n

        return r2

    putStrLn $ if ((okRows r) && (okColumns r) && (okSquares r))
        then "true"
        else "false"
        
    return ()
