import Data.List (sort)
splitListToInt :: String -> [String] -> [[Int]]
splitListToInt _ [] = []
splitListToInt delim [x]
    | x == delim = []
    | otherwise = [[read x :: Int]]
splitListToInt delim (x:xs)
    | x == delim = []: (r:rs)
    | otherwise = ((read x :: Int) : r) : rs
    where r:rs = splitListToInt delim xs

countMaxCalories :: [[Int]] -> Int
countMaxCalories elves = sum (take 3 (reverse (sort elvesCalories)))
    where elvesCalories = map sum elves

main = do
    contents <- readFile "input.txt"
    let result = countMaxCalories (splitListToInt "" (lines contents))
    print result