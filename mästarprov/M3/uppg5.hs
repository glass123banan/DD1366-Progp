-- Uppgift 5: Lambdakalkylen
-- Implementera en funktion som tar in en lista av heltal och 
-- returnerar det största heltalet i listan

-- KRAV:
--      1. Inga loopar (for, while)
--      2. ENDAST funktioner med lambdanotation (inkl. hjälpfunktioner)
--      3. Inga högre ordningens funktioner
--      4. Oföränderlighet, inga ändringar på variabel under samma anrop
--      5. Ha typsignaturer på alla funktioner 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use max" #-}

returnLargestInt :: [Integer] -> Integer
returnLargestInt [x] = x
returnLargestInt (x:xs) =
    (\a b -> if a > b then a else b) x (returnLargestInt xs)

string2List :: [Char] -> [Integer]
string2List inputString = map read (words inputString)

main::IO()
main = do
    -- let test = [3]
    -- let testRun = returnLargestInt test
    -- print testRun

    userInput <- getLine
    let userList = string2List userInput
    let userTest = returnLargestInt userList
    print userTest