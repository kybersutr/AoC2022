import Data.Map
import Data.Set
import Data.List

data File = File {name :: String,  size :: Int}
    deriving (Show, Eq, Ord)
type Directories = Map [String] (Set File)
type Path = [String]

insertToPath :: File -> Path -> Map [String] (Set File) -> Map [String] (Set File)
insertToPath f = Data.Map.adjust (Data.Set.insert f)

parseLine :: (Path, Map [String] (Set File)) -> String -> (Path, Map [String] (Set File))
parseLine (path, directories) "" = (path, directories)
parseLine (path, directories) line = case words line of
    ["$", "cd", ".."] -> (Prelude.take (length path - 1) path, directories)
    ["$", "cd", dir] -> (path ++ [dir], Data.Map.insert (path ++ [dir]) Data.Set.empty directories)
    ["dir", dir] -> (path, directories)
    ["$", "ls"] -> (path, directories)
    [size, name] -> (path, newDirectories directories path (File name (read size :: Int)))
        where
            newDirectories :: Map [String] (Set File) -> Path -> File -> Map [String] (Set File)
            newDirectories directories path file = Prelude.foldr (insertToPath file) directories (Prelude.drop 1 (inits path))
    _ -> (path, directories)

parseLines :: [String] -> Directories
parseLines inputLines = directories
    where
        (path, directories) = Prelude.foldl parseLine ([], Data.Map.empty) inputLines

sizes :: Directories -> Map [String] Int
sizes = Data.Map.map sumInside
    where
        sumInside :: Set File -> Int
        sumInside s = sum (Data.Set.map (\(File name size) -> size) s)

getAvailableSpace :: Int -> Map [String] Int-> Int
getAvailableSpace maxSpace d =
    case Data.Map.lookup ["/"] d of
        Just i -> maxSpace - i
        Nothing -> -1
main = do
    inputLines <- lines <$> readFile "input.txt"
    let directories = sizes $ parseLines inputLines
    let part1 = sum $ Data.Map.filter (<= 100000) directories
    print part1
    let availableSpace = getAvailableSpace 70000000 directories
    let part2 = minimum $ Data.Map.filter (\x -> availableSpace + x >= 30000000) directories 
    print part2