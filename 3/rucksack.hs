
import Data.Char (ord, isLower)
import Data.List (sort)
import System.Win32 (COORD(x))

elementPriority :: Char -> Int
elementPriority ch 
    | isLower ch = ord ch - 96
    | otherwise = ord ch - 38 

splitIntoTwo :: [Int] -> ([Int], [Int])
splitIntoTwo items = splitAt (length items `div` 2) items

sortItems :: ([Int], [Int]) -> ([Int], [Int])
sortItems (a,b) = (sort a, sort b)


findCommon :: ([Int], [Int]) -> Int
findCommon (a,b) = findCommonInner a b
    where 
        findCommonInner :: [Int] -> [Int] -> Int
        findCommonInner (x:xs) (y:ys)
            | x == y = x
            | x < y = findCommonInner xs (y:ys)
            | x > y = findCommonInner (x:xs) ys
        findCommonInner _ _ = -1

findCommonTriple :: ([Int], [Int], [Int]) -> Int
findCommonTriple (a,b,c) = findCommonTripleInner a b c
    where
        findCommonTripleInner :: [Int] -> [Int] -> [Int] -> Int
        findCommonTripleInner (x:xs) (y:ys) (z:zs) =
            case minimum [x,y,z] of
                min | (min == x) && (min == y) && (min == z) -> x
                    | min == x -> findCommonTripleInner xs (y:ys) (z:zs)
                    | min == y -> findCommonTripleInner (x:xs) ys (z:zs)
                    | min == z -> findCommonTripleInner (x:xs) (y:ys) zs
                _ -> -1
        findCommonTripleInner _ _ _ = -1


getRucksackPriority :: String -> Int 
getRucksackPriority items = findCommon $ sortItems $ splitIntoTwo $ map elementPriority items

splitIntoTriples :: [[Int]] -> [([Int], [Int], [Int])]
splitIntoTriples [] = []
splitIntoTriples [x, y, z] = [(x,y,z)]
splitIntoTriples (x:y:z:rest) = (x,y,z) : splitIntoTriples rest
splitIntoTriples _ = []

getBadgesPriorities :: [String] -> [Int] 
getBadgesPriorities s =  map findCommonTriple (splitIntoTriples $ map (sort . map elementPriority) s)

main = do
    contents <- readFile "input.txt"
    let firstResult = sum $ map getRucksackPriority (lines contents)
    let secondResult = sum $ getBadgesPriorities (lines contents) 
    print firstResult
    print secondResult
