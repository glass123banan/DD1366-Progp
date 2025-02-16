-- Return only the longest string in a list of strings
-- Uses foldl with accumulator starting as "". Compares length using if-else
longestString :: [String] -> String
longestString v = foldl (\acc x -> if length acc >= length x then acc else x) "" v

main :: IO()
main = do
    let a = ["hello", "hellu", "helle", "1"]
    print a
    let result = longestString a
    print result