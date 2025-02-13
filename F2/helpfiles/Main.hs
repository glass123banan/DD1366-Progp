import F2

main :: IO()
main = do
    -- Create DNA sequences
    let seq1 = string2seq "Seq1" "ATGC"
    let seq2 = string2seq "Seq2" "ATGA"
    let seq3 = string2seq "Seq3" "ATGG"

    -- Create Protein sequences
    let prot1 = string2seq "Prot1" "ARND"
    let prot2 = string2seq "Prot2" "ARNE"
    
    -- Generate profiles
    let dnaProfile1 = molseqs2profile "DNA Profile 1" [seq1, seq2]
    let dnaProfile2 = molseqs2profile "DNA Profile 2" [seq2, seq3]
        
    let protProfile1 = molseqs2profile "Protein Profile 1" [prot1]
    let protProfile2 = molseqs2profile "Protein Profile 2" [prot2]

     -- Compute and print distance matrix for MolSeq
    putStrLn "DNA Sequence Distance Matrix:"
    let dnaDistanceMatrix = distanceMatrix [seq1, seq2, seq3]
    mapM_ print dnaDistanceMatrix

    putStrLn "\nProtein Sequence Distance Matrix:"
    let proteinDistanceMatrix = distanceMatrix [prot1, prot2]
    mapM_ print proteinDistanceMatrix

    -- Compute and print distance matrix for Profiles
    putStrLn "\nProfile Distance Matrix:"
    let profileDistanceMatrix = distanceMatrix [dnaProfile1, dnaProfile2, protProfile1, protProfile2]
    mapM_ print profileDistanceMatrix

    -- Print profiles
    print dnaProfile1
    -- print dnaProfile2

    -- print protProfile1
    -- print protProfile2

    -- Compute profile distances
    -- let dnaDist = profileDistance dnaProfile1 dnaProfile2
    -- let protDist = profileDistance protProfile1 protProfile2

    -- -- Print results
    -- putStrLn $ "DNA Profile Distance: " ++ show dnaDist
    -- putStrLn $ "Protein Profile Distance: " ++ show protDist
