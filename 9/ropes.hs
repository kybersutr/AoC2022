import Data.List (foldl')
import Data.Set (fromList)
type Position = (Int, Int)
data Direction = U | D | L | R
    deriving (Show, Eq, Ord)
data Move = Move Direction Int
    deriving (Show, Eq, Ord)

parseDirection :: String -> Move
parseDirection s = Move direction (read (split !! 1) :: Int)
    where
        split = words s
        direction = case split !! 0 of
            "U" -> U
            "D" -> D
            "L" -> L
            "R" -> R
            _ -> U

moveHead :: Position -> Direction -> Position
moveHead (x,y) U = (x, y+1)
moveHead (x,y) D = (x, y-1)
moveHead (x,y) L = (x-1, y)
moveHead (x,y) R = (x+1, y)

moveTail :: Position -> Position -> Position
moveTail (hx, hy) (tx, ty)
    | (abs(tx - hx) <= 1) && (abs(ty - hy) <= 1) = (tx, ty)
    | (tx == hx) && (hy > ty) = (tx, ty+1)
    | (tx == hx) && (hy < ty) = (tx, ty-1)
    | (ty == hy) && (hx > tx) = (tx + 1, ty)
    | (ty == hy) && (hx < tx) = (tx - 1, ty)
    | (hx > tx) && (hy > ty) = (tx+1, ty+1)
    | (hx > tx) && (hy < ty) = (tx+1, ty-1)
    | (hx < tx) && (hy < ty) = (tx-1, ty-1)
    | (hx < tx) && (hy > ty) = (tx-1, ty+1)
    | otherwise = (tx, ty)

oneMove :: Direction -> (Position, [Position], [(Position, [Position])]) -> Int -> (Position, [Position], [(Position, [Position])])
oneMove dir (headPos, tailPos, history) _ = (newHeadPos, newTailPos, (newHeadPos, newTailPos): history)
    where
        newHeadPos = moveHead headPos dir
        newTailPos = tail $ scanl moveTail newHeadPos tailPos

innerFold :: (Position, [Position], [(Position, [Position])]) -> Move -> (Position, [Position], [(Position,[Position])])
innerFold (head, tail, visited) (Move dir count) = foldl' (oneMove dir) (head, tail, visited) [0..(count-1)]

getPositions :: [Move] -> Int -> [(Position, [Position])]
getPositions [] _ = []
getPositions moves n = positions
    where
        initialHead = (0,0)
        initialTails = replicate (n-1) (0,0)
        (headPos, tailPos, positions) = foldl' innerFold  (initialHead, initialTails, [(initialHead, initialTails)]) moves

main = do
    inputLines <- lines <$> readFile "input.txt"
    let directions = map parseDirection inputLines
    let part1 = length $ fromList $ map (last . snd) (getPositions directions 2)
    let part2 = length $ fromList $ map (last . snd) (getPositions directions 10)
    print "part1"
    print part1
    print "part2"
    print part2