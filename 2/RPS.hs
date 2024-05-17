decodeOpponentOne :: [String] -> [String]
decodeOpponentOne ["A", p] = ["R", p]
decodeOpponentOne ["B", p] = ["P", p]
decodeOpponentOne ["C", p] = ["S", p]
decodeOpponentOne cokolivJinyho = cokolivJinyho

decodeOpponent :: [[String]] -> [[String]]
decodeOpponent = map decodeOpponentOne

decodePlayerOne :: [String] -> [String]
decodePlayerOne [o, "X"] = [o, "R"]
decodePlayerOne [o, "Y"] = [o, "P"]
decodePlayerOne [o, "Z"] = [o, "S"]
decodePlayerOne cokolivJinyho = cokolivJinyho

decodePlayer :: [[String]] -> [[String]]
decodePlayer = map decodePlayerOne

decodePlayerOneNew :: [String] -> [String]
decodePlayerOneNew ["R", "X"] = ["R", "S"]
decodePlayerOneNew ["R", "Y"] = ["R", "R"]
decodePlayerOneNew ["R", "Z"] = ["R", "P"]
decodePlayerOneNew ["P", "X"] = ["P", "R"]
decodePlayerOneNew ["P", "Y"] = ["P", "P"]
decodePlayerOneNew ["P", "Z"] = ["P", "S"]
decodePlayerOneNew ["S", "X"] = ["S", "P"]
decodePlayerOneNew ["S", "Y"] = ["S", "S"]
decodePlayerOneNew ["S", "Z"] = ["S", "R"]
decodePlayerOneNew cokolivJinyho = cokolivJinyho

decodePlayerNew :: [[String]] -> [[String]]
decodePlayerNew = map decodePlayerOneNew



countRound :: [String] -> Int
countRound ["R", "R"] = 4
countRound ["R", "P"] = 8
countRound ["R", "S"] = 3
countRound ["P", "R"] = 1
countRound ["P", "P"] = 5
countRound ["P", "S"] = 9
countRound ["S", "R"] = 7
countRound ["S", "P"] = 2
countRound ["S", "S"] = 6
countRound cokolivJinyho = 0



countScore :: [[String]] -> Int
countScore = sum.map countRound

main = do
    contents <- readFile "input.txt"
    let parsed = map words (lines contents)
    let result1 = countScore $ decodePlayer $ decodeOpponent parsed
    let result2 = countScore $ decodePlayerNew $ decodeOpponent parsed
    print result1
    print result2