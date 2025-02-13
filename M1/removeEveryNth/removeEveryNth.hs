-- ASSIGNMENT 2

-- Function that removes every Nth element in a list of Ints
-- Calls on a helper function 'positionCheck' which evaluates the list based on index
-- Empty list returns empty list -> Base case
removeEveryNth :: Int -> [Int] -> [Int]
removeEveryNth _ [] = [] 
removeEveryNth n v = positionCheck 1 n v []

-- Helper function that checks position of the element index
-- If the position is equal n, the counter is resetted to 1
-- otherwise, the recursive call will be on position incremented by 1
-- Reverses acc in base case since the accumulator is appended by head
positionCheck :: Int -> Int -> [Int] -> [Int] -> [Int]
positionCheck _ _ [] acc = reverse acc 
positionCheck pos n (x:xs) acc
    |pos == n = positionCheck 1 n xs acc
    |otherwise = positionCheck (pos+1) n xs (x:acc)

-- main func for testing
main :: IO()
main = do
    let a = [1..10]
    print a
    let result = removeEveryNth 3 a
    print result