import Data.Char ( ord )
import Data.List (elemIndex)
import Data.Maybe

data Direction = L | R | U | D

getHeight :: Char -> Int
getHeight 'S' = 0
getHeight 'E' = 25
getHeight x = ord x - ord 'a'

getNeighbor :: [String] -> (Int, Int) -> Direction -> (Int, Int)
getNeighbor plan (x,y) dir = if (newHeight - myHeight) <= 1 then neighbor else (-1, -1)
    where
        myHeight = getHeight (getIndex plan (x,y))
        neighbor = case dir of
            U -> (x, y-1)
            D -> (x, y+1)
            L -> (x-1, y)
            R -> (x+1,y)
        newHeight = if fst neighbor >= 0 && snd neighbor >= 0 && fst neighbor < length (head plan) && snd neighbor < length plan then getHeight (getIndex plan neighbor) else 42

getIndex :: [String] -> (Int, Int) -> Char
getIndex plan (x,y) = (plan !! y) !! x

mostViablePath :: [String] -> [[(Int, Int)]] -> [(Int, Int)]
mostViablePath plan possiblePaths = possiblePaths !! index
    where
        index = fromJust (elemIndex (minimum lengths) lengths)
        lengths = map (\path -> if null path || getIndex plan (last path) /= 'E' then 666666666 else length path) possiblePaths


findPath :: [String] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
findPath plan (x,y) visited
    | x < 0 || y < 0 || x >= length (head plan) || y >= length plan = [] -- mimo plán
    | (x,y) `elem` visited = [] -- už visited
    | getIndex plan (x,y) == 'E' = [(x,y)] -- E
    | otherwise = if null pathRest then [] else (x,y):pathRest -- ostatní
        where
            pathRest = mostViablePath plan [up, down, left, right]
            up = findPath plan (getNeighbor plan (x,y) U) ((x,y):visited)
            down = findPath plan (getNeighbor plan (x,y) D) ((x,y):visited)
            left = findPath plan (getNeighbor plan (x,y) L) ((x,y):visited)
            right = findPath plan (getNeighbor plan (x,y) R) ((x,y):visited)

findS :: [String] -> Int -> (Int, Int)
findS [] _ = (-1, -1)
findS (p:ps) i
    | 'S' `elem` p = (i , fromJust (elemIndex 'S' p))
    | otherwise = findS ps (i+1)

main = do
    plan <- lines <$> readFile "rowOfLetters.txt"
    print plan
    print $ length $ findPath plan (findS plan 0) []