import Data.List (nub, sort)
import Data.Set (Set, fromList)
import System.Win32 (RegInfoKey(max_value_len))

data Beacon = Beacon Int Int
    deriving (Show)
data Sensor = Sensor Int Int Beacon Int
    deriving (Show)

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (a,b) (x,y) = abs (x - a) + abs (y - b)

parseSensor :: String -> Sensor
parseSensor s = Sensor x y beacon distance
    where
        w = words s
        x = read (init $ drop 2 (w !! 2)) :: Int
        y = read (init $ drop 2 (w !! 3)) :: Int
        beacon_x = read (init $ drop 2 (w !! 8)) :: Int
        beacon_y = read (drop 2 (w !! 9)) :: Int
        beacon = Beacon beacon_x beacon_y
        distance = manhattanDistance (x, y) (beacon_x, beacon_y)

canContainBeacon :: [Sensor] -> (Int, Int) -> Bool
canContainBeacon sensors (x,y) = all (sensorOk (x,y)) sensors
    where
        sensorOk (a,b) (Sensor k l _ distance) = manhattanDistance (a,b) (k,l) > distance

blockingBeaconsInRow :: Int -> Sensor -> [(Int,Int)]
blockingBeaconsInRow row (Sensor x y (Beacon bx by) distance) = filter (uncurry (<=)) result
    where
        firstComponent = abs(y - row)
        max_x_distance = distance - firstComponent
        left_barrier = x - max_x_distance
        right_barrier = x + max_x_distance
        result
            | (by == row) && (bx >= left_barrier) && (bx <= right_barrier) =
                  [(x-max_x_distance, bx-1), (bx+1, x+ max_x_distance)]
            | otherwise = [(x - max_x_distance, x + max_x_distance)]

blocked :: Int -> Sensor -> [(Int,Int)]
blocked row (Sensor x y (Beacon bx by) distance) = filter (uncurry (<=)) result
    where
        firstComponent = abs(y - row)
        max_x_distance = distance - firstComponent
        left_barrier = x - max_x_distance
        right_barrier = x + max_x_distance
        result = [(x - max_x_distance, x + max_x_distance)]



filterBeacons :: Set (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
filterBeacons beacons = filter (`notElem` beacons)

connectIntervals :: [(Int, Int)] -> [(Int, Int)]
connectIntervals [] = []
connectIntervals [x] = [x]
connectIntervals ((a,b):(c,d):rest)
    | b >= d = connectIntervals ((a,b):rest)
    | b >= c = connectIntervals ((a,d):rest)
    | b == c-1 = connectIntervals ((a,d):rest)
    | otherwise = (a,b):connectIntervals ((c,d):rest)

intervalLength :: (Int, Int) -> Int
intervalLength (x,y) = y-x+1

noBeaconsInThisArea :: [Sensor] -> Int -> [(Int, Int)]
noBeaconsInThisArea sensors i =
    connectIntervals $ sort $ concatMap (blockingBeaconsInRow i) sensors

noNewBeaconsInThisArea :: [Sensor] -> Int -> [(Int, Int)]
noNewBeaconsInThisArea sensors i =
    connectIntervals $ sort $ concatMap (blocked i) sensors


clip :: Int -> [(Int, Int)] -> [(Int, Int)]
clip _ [] = []
clip maxValue ((a,b):rest)
    | a < 0 && b > maxValue = [(0, maxValue)]
    | a < 0 && b < 0 = clip maxValue rest
    | a > maxValue && b > maxValue = []
    | a < 0 && b <= maxValue = (0,b):clip maxValue rest
    | a >= 0 && b <= maxValue = (a,b):clip maxValue rest
    | a >= 0 && b > maxValue = (a, maxValue):clip maxValue rest
    | otherwise = []

findBeaconRow :: [(Int, Int)] -> Int
findBeaconRow [] = -1
findBeaconRow ((a,b):rest) = b + 1

findBeacon :: [(Int, [(Int, Int)])] -> Int -> (Int, Int)
findBeacon [] _ = (-1, -1)
findBeacon ((index, intervals):rest) maxValue
    | sum (map intervalLength intervals) == maxValue + 1 = findBeacon rest maxValue
    | otherwise = (findBeaconRow intervals, index)



main :: IO ()
main = do
    contents <- lines <$> readFile "input.txt"
    let sensors = map parseSensor contents
    let blocking = map (blockingBeaconsInRow 10) sensors
    let connected = connectIntervals $ sort $ concat blocking
    print connected
    print $ sum $ map intervalLength connected
    let maxValueMini = 20
    let maxValueReal = 4000000
    let noBeacons = map (clip maxValueReal . noNewBeaconsInThisArea sensors) [0..maxValueReal]
    print $ length noBeacons
    let (beaconx, beacony) = findBeacon (zip [0..(length noBeacons -1)] noBeacons) maxValueReal
    print (4000000*beaconx + beacony)
