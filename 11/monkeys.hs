{-# LANGUAGE ScopedTypeVariables #-}
import Data.List (foldl', elemIndex)

type Operation = (Int -> Int)
data Monke = Monke [Int] Operation Int Int Int

modulator :: [Monke] -> Int
modulator monkeys = product (map (\(Monke _ _ m _ _) -> m) monkeys)

basicMonke :: [Monke]
basicMonke =
    [
        Monke [1] (\(x::Int) -> x * 3) 11 3 3,
        Monke [2] (\(x::Int) -> x*3 ) 11 0 0,
        Monke [3] (\(x::Int) -> x*3) 11 0 0,
        Monke [4] (\(x::Int) -> x*3) 11 2 2
    ]

testMonke :: [Monke]
testMonke =
    [
        Monke [79, 98] (\(x::Int) -> x*19) 23 2 3,
        Monke [54, 65, 75, 74] (\(x::Int) -> x + 6) 19 2 0,
        Monke [79, 60, 97] (\(x::Int) -> x*x) 13 1 3,
        Monke [74] (\(x::Int) -> x+3) 17 0 1
    ]

realMonke :: [Monke]
realMonke =
    [
        Monke [54, 82, 90, 88, 86, 54] (\(x::Int) -> x*7) 11 2 6,
        Monke [91, 65] (\(x::Int) -> x*13) 5 7 4,
        Monke [62, 54, 57, 92, 83, 63, 63] (\(x::Int) -> x+1) 7 1 7,
        Monke [67, 72, 68] (\(x::Int) -> x*x) 2 0 6,
        Monke [68, 89, 90, 86, 84, 57, 72, 84] (\(x::Int) -> x+7) 17 3 5,
        Monke [79, 83, 64, 58] (\(x::Int) -> x+6) 13 3 0,
        Monke [96, 72, 89, 70, 88] (\(x::Int) -> x+4) 3 1 2,
        Monke [79] (\(x::Int) -> x+8) 19 4 5
    ]

throw :: [Monke] -> (Int, Int) -> [Monke]
throw monkeys (worry, newMonkeIndex) =
    newMonkeys
    where
        Monke items fun modulo t f = monkeys !! newMonkeIndex
        newMonke = Monke (worry:items) fun modulo t f
        (x,y:ys) = splitAt newMonkeIndex monkeys
        newMonkeys = x ++ [newMonke] ++ ys



updateOne :: ([Monke], [Int]) -> Int -> ([Monke], [Int])
updateOne (monkeys, counts) monkeIndex =
    (foldl' throw newMonkeys updates, newCounts)
    where
        Monke items fun modulo t f = monkeys !! monkeIndex
        newWorries = map ((`mod` modulator monkeys).fun) items
        throwTo = map (\x -> if x `mod` modulo == 0 then t else f) newWorries
        updates = zip newWorries throwTo
        monkeCount = length newWorries
        (a,b:bs) = splitAt monkeIndex counts
        newCounts = a ++ [b + monkeCount] ++ bs
        newMonke = Monke [] fun modulo t f
        (x,y:ys) = splitAt monkeIndex monkeys
        newMonkeys = x ++ [newMonke] ++ ys


monkeRound :: ([Monke], [Int]) -> ([Monke], [Int])
monkeRound (monkeys, counts) = foldl' updateOne (monkeys, counts) monkeysWithIndices
    where
        monkeysWithIndices = [0..(length monkeys - 1)]


main = do
    let monkeys = realMonke
    let monkeyBusiness = last $ take 10001 (iterate monkeRound (monkeys, replicate (length monkeys) 0))
    print (map (\(Monke a _ _ _ _) -> a)(fst monkeyBusiness))
    print (snd monkeyBusiness)