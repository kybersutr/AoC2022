import qualified Data.Set as Set
data Window = Window {chars :: String, offset :: Int, isDistinct :: Bool} deriving (Show, Eq)

updateWindow :: Window -> Char -> Window
updateWindow (Window windowString offset isDistinct) ch
    | isDistinct = Window windowString offset isDistinct
    | otherwise = Window newString (offset + 1) newIsDistinct
    where
        (x:xs) = windowString
        newString = xs ++ [ch]
        withoutDuplicates = Set.fromList newString
        newIsDistinct = length withoutDuplicates == length newString

getDistinctOffset :: String -> Int -> Int 
getDistinctOffset signal windowLength = lastOffset
    where
        Window _ lastOffset _ = foldl updateWindow (Window firstWindowString windowLength isDistinct)  (drop windowLength signal)
        firstWindowString = take windowLength signal
        withoutDuplicates = Set.fromList firstWindowString
        isDistinct = length withoutDuplicates == length firstWindowString

main = do
    contents <- readFile "input.txt"
    let result1 = getDistinctOffset contents 4
    let result2 = getDistinctOffset contents 14
    print result1
    print result2