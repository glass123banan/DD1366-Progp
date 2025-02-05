-- ASSIGNMENT 1

-- Main sirSimulate function, call on zip3 to create list of triples
-- Calls helper functions findSuceptible, findInfected, and findRecovered
-- For base case, only add initialstate directly in a list as a triple
sirSimulate :: Double -> Double -> Int -> Int -> (Int, Int, Int) -> [(Int, Int, Int)]
sirSimulate beta gamma n steps initialState  
    |steps == 0 = [initialState]
    |steps > 0 = zip3 
                    (init (findSuceptible beta gamma n steps initialState)) 
                    (init (findInfected beta gamma n steps initialState)) 
                    (init (findRecovered beta gamma n steps initialState))

-- Helper function that creates a list of integers for amount of suceptible
-- Uses recursion to find previous values and calculates with given formula
findSuceptible :: Double -> Double -> Int -> Int -> (Int, Int, Int) -> [Int]
findSuceptible beta gamma n steps (s, i, r)
    |steps == 0 = [s]
    |steps > 0 = 
        let prevList = findSuceptible beta gamma n (steps-1) (s, i, r)
            prevS = last prevList
            prevI = last (findInfected beta gamma n (steps-1) (s, i, r))
        in prevList ++ [round(fromIntegral prevS - beta * fromIntegral prevS * fromIntegral prevI/fromIntegral n)]

-- Helper function that creates a list of integers for amount of infected
-- Uses recursion to find previous values and calculates with given formula           
findInfected :: Double -> Double -> Int -> Int -> (Int, Int, Int) -> [Int]
findInfected beta gamma n steps (s, i, r)
    |steps == 0 = [i]
    |steps > 0 = 
        let prevList = findInfected beta gamma n (steps-1) (s, i, r)
            prevI = last prevList
            prevS = last (findSuceptible beta gamma n (steps-1) (s, i, r))
            prevR = last (findRecovered beta gamma n (steps-1) (s, i, r))
        in prevList ++ [round(fromIntegral prevI + beta * fromIntegral prevS * fromIntegral prevI/fromIntegral n - gamma * fromIntegral prevI)]

-- Helper function that creates a list of integers for amount of recovered
-- Uses recursion to find previous values and calculates with given formula   
findRecovered :: Double -> Double -> Int -> Int -> (Int, Int, Int) -> [Int]
findRecovered beta gamma n steps (s, i, r)
    |steps == 0 = [r]
    |steps > 0 = 
        let prevList = findRecovered beta gamma n (steps-1) (s, i, r)
            prevR = last prevList
            prevI = last (findInfected beta gamma n (steps-1) (s, i, r))
        in prevList ++ [round(fromIntegral prevR + gamma * fromIntegral prevI)]

main :: IO()
main = do
    -- let result1 = sirSimulate 0.3 0.1 1000 10 (990, 10, 0)
    -- print result1

    -- let result2 = sirSimulate 0.1 0.05 5000 10 (4950, 50, 0)
    -- print result2

    -- let result3 = sirSimulate 0.8 0.4 10000 10 (9990, 10, 0)
    -- print result3

    let result4 = sirSimulate 0.2 0.01 10000 100 (9990, 10, 0)
    print result4   
