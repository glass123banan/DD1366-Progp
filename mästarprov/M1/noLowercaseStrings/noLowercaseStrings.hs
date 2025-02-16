-- ASSIGNMENT 3

-- Remove every string in the list of string that contains ANY lowercase letter
-- Uses filter to apply on all elements in v to not contain elements with lowercase letters 
noLowercaseStrings :: [String] -> [String]
noLowercaseStrings v = filter (all (`notElem` "abcdefghijklmnopqrstuvwxyzåäö")) v 

-- main func for testing
main :: IO()
main = do
    let a = ["HELLO", "hello", "123", "OMg"]
    print a
    let result = noLowercaseStrings a
    print result