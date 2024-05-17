import Data.Maybe
type Crate = Char
type CrateStack = [Crate]
type Cargo = [CrateStack]
data Move = Move {count :: Int, from :: Int, to :: Int} -- reindex to indexing from zero while parsing
type Moves = [Move]

moveOne :: Cargo -> Int -> Int -> Cargo
moveOne cargo from to = 
    map (newCargo from to crateToMove) [0..(length cargo-1)]
    where
        (crateToMove:_) = cargo !! from
        newCargo :: Int -> Int -> Crate -> Int -> CrateStack
        newCargo from to crate index
            | index == from = drop 1 (cargo !! index)
            | index == to = crate:(cargo !! index)
            | otherwise = cargo !! index


move :: Cargo -> Move -> Cargo
move cargo (Move 0 _ _) = cargo
move cargo (Move count from to) = move (moveOne cargo from to) (Move (count-1) from to)

craneOperations :: Cargo -> Moves -> Cargo
craneOperations initialState moves = foldl move initialState moves

move2 :: Cargo -> Move -> Cargo
move2 cargo (Move count from to) = 
    map (newCargo from to cratesToMove) [0..(length cargo-1)]
    where
        cratesToMove = take count (cargo !! from)
        newCargo :: Int -> Int -> [Crate] -> Int -> CrateStack
        newCargo from to crates index
            | index == from = drop count (cargo !! index)
            | index == to = crates ++ (cargo !! index)
            | otherwise = cargo !! index


craneOperations2 :: Cargo -> Moves -> Cargo
craneOperations2 initialState moves = foldl move2 initialState moves

getTop :: CrateStack -> Char
getTop [] = ' '
getTop (x:xs) = x

cargo :: Cargo
cargo = 
    [ 
        "WTHPJCF",
        "HBHZFVRG",
        "RTPH",
        "THPNSZ",
        "DCJHZFVN",
        "ZDWFGMP",
        "PDJSWZVM",
        "SDN",
        "MFSZD"
    ]

getMove :: String -> Move
getMove s = 
    Move count from to
    where
        w = words s
        count = read (w !! 1) :: Int
        from = (read (w !! 3) :: Int) - 1
        to = (read (w !! 5) :: Int) - 1

main :: IO ()
main = do
    contents <-  readFile "input.txt"
    let relevant = drop 10 (lines contents)
    let moves = map getMove relevant
    let result = map getTop (craneOperations cargo moves)
    print result
    let result2 = map getTop (craneOperations2 cargo moves) 
    print result2