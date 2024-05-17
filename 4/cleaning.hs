import Text.ParserCombinators.Parsec
import Data.Either

data Range = Range {start :: Int, end :: Int}
    deriving (Show)

overlapContains :: (Range, Range) -> Bool
overlapContains (Range s1 e1, Range s2 e2) =
    (s1 <= s2 && e2 <= e1) || (s2 <= s1) && (e1 <= e2) 

countOverlaps :: [(Range,Range)] -> Int
countOverlaps = foldl (\i pair -> if overlapContains pair then i+1 else i) 0

overlapContains2 :: (Range, Range) -> Bool
overlapContains2 (Range s1 e1, Range s2 e2) =
    (s1 <= s2 && e1 >= s2) || (s2 <= s1 && e2 >= s1)


countOverlaps2 :: [(Range, Range)] -> Int
countOverlaps2 = foldl (\i pair -> if overlapContains2 pair then i+1 else i) 0

main = do
    input <- readFile "input.txt"
    let ranges = fromRight [] (parseInput input)
    print (countOverlaps2 ranges)

line :: GenParser Char st (Range, Range)
line = do
    start1 <- many1 digit
    _ <- char '-'
    end1 <- many1 digit
    _ <- char ','
    start2 <- many1 digit
    _ <- char '-'
    end2 <- many1 digit
    _ <- char '\n'
    let range1 = Range (read start1 :: Int) (read end1 :: Int)
    let range2 = Range (read start2 :: Int) (read end2 :: Int)
    return (range1, range2)

inputString :: GenParser Char st [(Range, Range)]
inputString = do
    result <- many line
    eof
    return result

parseInput :: String -> Either ParseError [(Range, Range)]
parseInput = parse inputString "Error"
