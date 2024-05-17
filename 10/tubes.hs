import Data.List (foldl')

type Count = Int
type Value = Int
type Strength = Int
data Operation = Operation Count Value
    deriving (Show, Eq, Ord)

parseOp :: String -> Operation
parseOp s = case words s of
  ["noop"] -> Operation 1 0
  ["addx", i] -> Operation 2 (read i :: Int)
  _ -> Operation 0 0

overlapping :: Count -> Value -> Int
overlapping c v = if abs(c-v) <= 1 then 1 else 0

doOperation :: (Count, Value, [Strength]) -> Operation -> (Count, Value, [Strength])
doOperation (c, v, strengths) (Operation count value) =
    (c + count, v + value, newStrengths)
    where
        --newStrengths
        --    | (c-20) `mod` 40 == 0 = (c * v) : strengths
        --    | (((c+1) - 20) `mod` 40 == 0) && (count == 2) = ((c+1) * v) : strengths
        --    | otherwise = strengths

        newStrengths
            | count == 1 = overlapping ((c-1) `mod` 40) v : strengths
            | otherwise = overlapping (c `mod` 40) v : overlapping ((c-1) `mod` 40) v : strengths


doOperations :: (Count, Value, [Strength]) -> [Operation] -> (Count, Value, [Strength])
doOperations = foldl' doOperation

toDraw :: Int -> Char
toDraw 0 = ' '
toDraw 1 = '#'
toDraw _ = '_'

main = do
    operations <- map parseOp . lines <$> readFile "input.txt"
    let (last_count, last_value, signals)= doOperations (1,1,[]) operations
    -- let part1 = sum signals
    let part2 = map toDraw $ reverse signals
    print (take 40 part2)
    print (take 40 (drop 40 part2))
    print (take 40 (drop 80 part2))
    print (take 40 (drop 120 part2))
    print (take 40 (drop 160 part2))
    print (take 40 (drop 200 part2))