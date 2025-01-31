-- type declaration for removeNth cuz it takes in parameters list(v) and position(n) and returns list of ints
removeNth :: Int -> [Int] -> [Int]
removeNth _ [] = []
removeNth n v = positionCheck 1 n v

-- helper function that checks which position in the list we are at
positionCheck :: Int -> Int -> [Int] -> [Int]
positionCheck _ _ [] = []
positionCheck pos n (x:xs)
    |pos `mod` n == 0 = positionCheck 1 n xs
    |otherwise = x : positionCheck (pos+1) n xs

-- main func for testing
main :: IO()
main = do
    let a = [1, 2, 3]
    print a
    let result = removeNth 1 a
    print result