import Data.Text (splitOn, pack, unpack)
data Thing = N Int | Empty | List [Thing]
    deriving (Show, Eq, Ord, Read)

inRightOrder :: (Thing, Thing) -> Bool
inRightOrder (_, _) = True
inRightOrder (_, _) = False
inRightOrder (_, _) = True

readThing :: String -> Thing
readThing "[]" = Empty
readThing ('[':rest) = List [(readThing (take (length rest - 2) rest))]
readThing _ = N 0  

separate :: [a] -> (a, a)
separate x = (head x, last x)

main :: IO ()
main = do
    contents <- pack <$> readFile "mini_input.txt"
    let pairs = map (separate . map readThing . lines . unpack) (splitOn (pack "\n\n") contents)
    print pairs
    --let comparisons = map inRightOrder pairs
    -- print comparisons