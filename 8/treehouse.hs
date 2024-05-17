import Data.List (foldl', transpose)

data Tree = Tree {height :: Int, visibilities :: [Bool], distances :: [Int]} -- Left, Right, Top, Down
    deriving (Show, Eq)

type Forest = [[Tree]]

-------------------------------------------------------------------------------------
-- First part --

innerFold :: Int -> ([Tree], Int) -> Tree -> ([Tree], Int)
innerFold vIndex (currentTrees, maxHeight) (Tree height visibilities distances)
    | height > maxHeight = (Tree height visibilities distances : currentTrees, height)
    | otherwise = (Tree height newVisibilities distances : currentTrees, maxHeight)
        where
            (x, _: xs) = splitAt vIndex visibilities
            newVisibilities = x ++ False : xs

checkRow :: Int -> ([Tree] -> ([Tree], Int))
checkRow v = foldl' (innerFold v) ([], -1)

-------------------------------------------------------------------------------------
-- Second part fixed --
innerFold2 :: Int -> ([Tree], [(Int, Int)]) -> Tree -> ([Tree], [(Int, Int)])
-- [(Int, Int)] is list of length of preceding rising subsequences and their last elements
innerFold2 vIndex (currentTrees, risingSubsequences) (Tree height v distances) =
    (Tree height v newDistances : currentTrees, newRisingSubsequences)
    where
        precedingIntervalsLengths = map fst (takeWhile (\(_,b) -> height > b) risingSubsequences)
        myDistance
                | length precedingIntervalsLengths == length risingSubsequences = sum precedingIntervalsLengths
                | otherwise = sum precedingIntervalsLengths + 1
        (x,_:xs) = splitAt vIndex distances
        newDistances = x ++ myDistance : xs

        ((risingLength, risingLastHeight):ys) = risingSubsequences
        newRisingSubsequences
            | risingLastHeight < height = (risingLength + 1, height) : ys
            | otherwise = (1, height) : ((risingLength, risingLastHeight):ys)

scenicRow :: Int -> ([Tree] -> ([Tree], [(Int, Int)]))
scenicRow v = foldl' (innerFold2 v) ([], [(0,0)])
-------------------------------------------------------------------------------------

checkVisibilityLRRow :: [Tree] -> [Tree]
checkVisibilityLRRow row = fst (scenicRow 1 (fst (scenicRow 0 (fst (checkRow 1 (fst (checkRow 0 row)))))))
-- kinda hack, foldl' reverses the list of trees

checkVisibilityLR :: Forest -> Forest
checkVisibilityLR = map checkVisibilityLRRow

checkVisibilityTBRow :: [Tree] -> [Tree]
checkVisibilityTBRow row = fst( scenicRow 3 (fst (scenicRow 2 (fst (checkRow 3 (fst (checkRow 2 row)))))))

checkVisibilityTB :: Forest -> Forest
checkVisibilityTB f = transpose (map checkVisibilityTBRow (transpose f))

checkVisibility :: Forest -> Forest
checkVisibility = checkVisibilityTB . checkVisibilityLR

visible :: Tree -> Bool
visible (Tree _ v _) = or v

countTrue :: [[Bool]] -> Int
countTrue rows = sum (map (length . filter (==True)) rows)

scenicScore :: Tree -> Int
scenicScore (Tree  _ _ d) = product d

parseRows :: [String] -> Forest
parseRows = map parseRow
    where
        parseRow :: String -> [Tree]
        parseRow "" = []
        parseRow (x:xs) = Tree (read [x] :: Int) [True, True, True, True] [0, 0, 0, 0] : parseRow xs


main = do
    rows <- lines <$> readFile "input.txt"
    let checkedTrees = checkVisibility $ parseRows rows
    let part1 = countTrue (map (map visible) checkedTrees)
    let scenicScores = map (map scenicScore) checkedTrees
    let part2 = maximum (map maximum scenicScores)
    print part1
    print part2