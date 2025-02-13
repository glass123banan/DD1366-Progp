module F2 where

-- Import for help code from pdf
import Data.List (sort, group, transpose, unionBy, tails)

-- Creating datatype MolSeq with sequence name and sequence (both strings)
data MolSeq = DNA { molseq_name::String, molseq_sequence::String }
            | Protein { molseq_name::String, molseq_sequence::String }
    deriving (Show) -- so it can be printed neatly

-- Converts a name and a sequence into MolSeq type
-- Uses 'all' to check whether each element in the sequence belongs to specified letter-type
string2seq :: String -> String -> MolSeq
string2seq sequenceName actualSequence
    |all (`elem` "atgcATGC") actualSequence = DNA sequenceName actualSequence
    |all (`elem` "ARNDCEQGHILKMFPSTWYVarndceqghilkmfpstwyv") actualSequence = Protein sequenceName actualSequence
    |otherwise = error "Characters not valid in sequence"

-- Return the name of the MolSeq, return the 'field' called molseq_name
seqName :: MolSeq -> String
seqName = molseq_name

-- Return the sequence of the MolSeq, return the 'field' called molseq_sequence
seqSequence :: MolSeq -> String
seqSequence = molseq_sequence

-- Return the length of the MolSeq sequence
-- First return the sequence itself ('field') and THEN apply length on it 
seqLength :: MolSeq -> Int
seqLength = length . molseq_sequence

-- Function that returns the distance between two MolSeqs
-- Equation differs depending on if its DNA or Protein that is compared
seqDistance :: MolSeq -> MolSeq -> Double
-- Pattern match two DNA inputs and compute with helper funcs getAlpha and countSeqMismatch
seqDistance (DNA name1 seq1) (DNA name2 seq2)
        | alpha >= 0 && alpha < 0.74 = - (3 / 4 * log (1 - 4 * alpha / 3))
        | otherwise = 3.3
    -- Tells us where we get the variables alpha from
    where
        mismatches = countSeqMismatch seq1 seq2
        alpha = getAlpha seq1 mismatches
-- Pattern match two Protein inputs and compute with helper funcs getAlpha and countSeqMismatch
seqDistance (Protein name1 seq1) (Protein name2 seq2)
        | alpha >= 0 && alpha <= 0.94 = - (19 / 20 * log (1 - 20 * alpha / 19))
        | otherwise = 3.7
    where
        mismatches = countSeqMismatch seq1 seq2
        alpha = getAlpha seq1 mismatches

-- Error case: If any of the above cases aren't met, we are comparing a DNA with a Protein
-- This results in an error 
seqDistance _ _ = error "Cannot compare DNA with Protein"

-- Helper func to find amount of mismatches in the two strings
countSeqMismatch :: String -> String -> Double
countSeqMismatch [] [] = 0  -- Base case: no mismatches in empty sequences
countSeqMismatch (x:xs) (y:ys) -- Recursive case: two lists x:xs and y:ys
    | x == y = countSeqMismatch xs ys -- If the heads are identical, skip and call itself again
    | otherwise = 1 + countSeqMismatch xs ys -- If heads non-identical, add 1 to result and call itself

-- Helper func to find percentage of mismatches (alpha)
getAlpha :: String -> Double -> Double
-- Percentage = total amount of mismatches / length of sequence
getAlpha seq mismatches = mismatches/fromIntegral (length seq) -- fromIntegral transforms into num-type

-- Find the type of MolSeq it is 
seqType :: MolSeq -> String
seqType (DNA _ _ ) = "DNA"
seqType (Protein _ _ ) = "Protein"

-- Hjälpkod från uppgiftsbeskrivning för att skapa profil matris
nucleotides = "ACGT"
aminoacids = sort "ARNDCEQGHILKMFPSTWYV"
makeProfileMatrix :: [MolSeq] -> [[(Char, Double)]]
makeProfileMatrix [] = error "Empty sequence list"
makeProfileMatrix startlist = res
    where
        t = seqType (head startlist) -- Find the type of MolSeq (DNA/Protein)
        defaults =
            -- If MolSeq-type is DNA, initialize list of tuples with nucleotides ACGT and zeros
            if t == "DNA" then
                zip nucleotides (replicate (length nucleotides) 0)
            -- Else, initialize list of tuples with aminoacids and zeros
            else
                zip aminoacids (replicate (length aminoacids) 0)
        extractSeqList = map seqSequence startlist  -- Apply seqSequence on all startlist elements (MolSeqs) to find sequences                            
        -- Transpose so that the sequences line up in columns instead so each row is element at pos 1 in each seq
        -- Sort in order to order same letters next to each other, group into lists of identical letters
        --      ex. ['A', 'C', 'A', 'C'] -> sort: ['A', 'A', 'C', 'C'] -> group: [['A', 'A'], ['C', 'C']]
        -- Map finds head of each element (the Char) 
        --      and computes freq dividing length of list ['A', 'A'] with amount of MolSeqs in list
        tmp1 = map (map (\x -> (head x, fromIntegral (length x)/fromIntegral (length startlist))) . group . sort)
            (transpose extractSeqList)
        -- Func that checks if two tuples has the same first element (Char in this case)
        equalFst a b = fst a == fst b
        -- Ensure that each possible character (nucleotide or aminoacid) is included in the profile matrix.  
        -- 'unionBy equalFst' merges 'tmp1' (which contains observed frequencies) with 'defaults'  
        -- (which provides a default frequency of 0.0 for missing characters).  
        -- Sorting ensures a consistent order in the final result. 
        res = map (sort . (\l -> unionBy equalFst l defaults)) tmp1


-- Datatype profile 
data Profile = Profile {
    profilename :: String,
    seqtype :: String,
    seqCount :: Int,
    profilenMatrix :: [[(Char, Double)]]
} deriving (Show)

-- Create profile from list of MolSeqs 
molseqs2profile :: String -> [MolSeq] -> Profile
molseqs2profile name molseqList = Profile name seqtype (length molseqList) profileMatrix
    where
        seqtype = seqType (head molseqList) -- Profile type is the same as head of first elem in list
        profileMatrix = makeProfileMatrix molseqList -- Call help func using input list

-- Return name of profile directly from field
profileName :: Profile -> String
profileName = profilename

-- Return relative frequency of a specific letter in matrix
profileFrequency :: Profile -> Int -> Char -> Double
profileFrequency (Profile name seqtype seqcount matrix) index char = res
    where
        -- matrix!!index -> List of tuples at given index
        -- Lambdafunc to pattern match the given character at the correct list
        tuple = filter (\(c, _) -> c == char) (matrix!!index) 
        -- Find the frequency by taking second elem in tuple
        res = snd (head tuple)

-- Calculating profile distance formula
profileDistance :: Profile -> Profile -> Double
profileDistance (Profile name1 seqtype1 seqcount1 matrix1) (Profile name2 seqtype2 seqcount2 matrix2) = res
    where
        m1Lst = concatMap (map snd) matrix1 -- Take out the second element in tuple in each list in matrix1
        m2Lst = concatMap (map snd) matrix2 -- Same as above but with matrix2
        diffList = zipWith (\x y -> abs (x-y)) m1Lst m2Lst -- List with diff between list 1 and 2
        res = sum diffList -- Set res as the sum of the difference

-- Typeclass Evol with instances of MolSeq and Profile (like an interface)
-- 'a' is the instace => MolSeq or Profile in this case
class Evol a where
    distance :: a -> a -> Double
    name :: a -> String
    distanceMatrix :: [a] -> [(String, String, Double)]

-- Instance MolSeq
instance Evol MolSeq where
    -- Same method as seqDistance func
    distance :: MolSeq -> MolSeq -> Double
    distance (DNA name1 seq1) (DNA name2 seq2)
            | alpha >= 0 && alpha < 0.74 = - (3 / 4 * log (1 - 4 * alpha / 3))
            | otherwise = 3.3
        where
            mismatches = countSeqMismatch seq1 seq2
            alpha = getAlpha seq1 mismatches
    distance (Protein name1 seq1) (Protein name2 seq2)
            | alpha >= 0 && alpha <= 0.94 = - (19 / 20 * log (1 - 20 * alpha / 19))
            | otherwise = 3.7
        where
            mismatches = countSeqMismatch seq1 seq2
            alpha = getAlpha seq1 mismatches
    distance _ _ = error "Cannot compare DNA with Protein"

    -- Same method as seqName method
    name :: MolSeq -> String
    name = molseq_name

    -- Returns all pairs of distances   
    distanceMatrix :: [MolSeq] -> [(String, String, Double)]
    -- Computes a distance matrix for a list of molecular sequences.  
    -- Returns a list of triples (seq1_name, seq2_name, distance) for all pairs of sequences.  
    --  
    -- List comprehension explanation:  
    --      EXAMPLE: [(i,j) | i <- [1,2],
    --                            j <- [1..4] ]
    --      `(seq1:rest) <- tails seqs` iterates over all non-empty suffixes (tails) of `seqs`,  
    --      ensuring that each sequence `seq1` is paired only with those appearing later in the list (to avoid duplicates).  
    --      `seq2 <- seq1:rest` iterates over `seq1` and the remaining sequences in `rest`, forming all valid pairs.  
    --      `(name seq1, name seq2, distance seq1 seq2)` constructs the tuple using predefined functions:  
    --      `name` extracts the sequence identifier.  
    --      `distance` computes the distance between the two sequences.  
    distanceMatrix seqs =
        [ (name seq1, name seq2, distance seq1 seq2) | (seq1:rest) <- tails seqs, 
                                                        seq2 <- seq1:rest ]

-- Instance of Profile    
instance Evol Profile where
    -- Same as above
    distance :: Profile -> Profile -> Double
    distance (Profile name1 seqtype1 seqcount1 matrix1) (Profile name2 seqtype2 seqcount2 matrix2) = res
        where
            m1Lst = concatMap (map snd) matrix1
            m2Lst = concatMap (map snd) matrix2
            diffList = zipWith (\x y -> abs (x-y)) m1Lst m2Lst
            res = sum diffList

    -- Same as above
    name :: Profile -> String
    name = profilename

    -- Same as above
    distanceMatrix :: [Profile] -> [(String, String, Double)]
    distanceMatrix profiles =
        [ (name profileA, name profileB, distance profileA profileB)
        | (profileA:rest) <- tails profiles, profileB <- profileA:rest ]
