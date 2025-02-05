-- ASSIGNMENT 1

-- 
sirSimulate :: Double -> Double -> Int -> Int -> (Int, Int, Int) -> [(Int, Int, Int)]
sirSimulate beta gamma n steps initialState  
    |steps == 0 = [initialState]
    |steps > 0 = zip3 
                    (init (findSuceptible beta gamma n steps initialState)) 
                    (init (findInfected beta gamma n steps initialState)) 
                    (init (findRecovered beta gamma n steps initialState))
    
findSuceptible :: Double -> Double -> Int -> Int -> (Int, Int, Int) -> [Int]
findSuceptible beta gamma n steps (s, i, r)
    |steps == 0 = [s]
    |steps > 0 = 
        let prevList = findSuceptible beta gamma n (steps-1) (s, i, r)
            prevS = last prevList
            prevI = last (findInfected beta gamma n (steps-1) (s, i, r))
        in prevList ++ [round(fromIntegral prevS - beta * fromIntegral prevS * fromIntegral prevI/fromIntegral n)]
            
findInfected :: Double -> Double -> Int -> Int -> (Int, Int, Int) -> [Int]
findInfected beta gamma n steps (s, i, r)
    |steps == 0 = [i]
    |steps > 0 = 
        let prevList = findInfected beta gamma n (steps-1) (s, i, r)
            prevI = last prevList
            prevS = last (findSuceptible beta gamma n (steps-1) (s, i, r))
            prevR = last (findRecovered beta gamma n (steps-1) (s, i, r))
        in prevList ++ [round(fromIntegral prevI + beta * fromIntegral prevS * fromIntegral prevI/fromIntegral n - gamma * fromIntegral prevI)]

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
