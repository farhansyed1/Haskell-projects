module F2 where

import Data.List
import Data.Char

-- Authors: Farhan Syed & Abhinav Sasikumar   
-- Date: 2023-02-01

-- Exercise 2.1
-- Data types
data TypeOfSequence = DNA | Protein deriving (Show,Eq) 

data MolSeq = MolSeq{
    sequenceName :: String,
    sequence :: String,
    sequenceType :: TypeOfSequence
} deriving (Show, Eq)

-- Exercise 2.2
-- Checks that all letters in the given sequences are DNA letters. If True -> DNA. Else -> Protein
dnaLetters :: String -> Bool
dnaLetters [] = True
dnaLetters (x:xs) = x `elem` "ACGT" && dnaLetters xs 

string2seq :: String -> String -> MolSeq
string2seq a b 
    | dnaLetters b = MolSeq a b DNA         -- Return a MolSeq with type DNA if dnaLetters is True
    | otherwise = MolSeq a b Protein        -- Return a MolSeq with type Protein if dnaLetters is False

-- Exercise 2.3
-- Returns the MolSeq's name
seqName :: MolSeq -> String
seqName (MolSeq a _ _) = a 

-- Returns the MolSeq's sequence
seqSequence :: MolSeq -> String
seqSequence (MolSeq _ b _) = b

-- Returns the MolSeq's sequence length
seqLength :: MolSeq -> Int
seqLength (MolSeq _ b _ ) = length b 

-- Exercise 2.4
-- Returns the MolSeq's type (DNA or Protein)
seqType :: MolSeq -> TypeOfSequence
seqType (MolSeq _ _ c ) = c

-- Two helper functions that take in an alpha value and return the distance
evDistanceDNA :: Double -> Double
evDistanceDNA alpha
        | alpha > 0.74 = 3.3
        | otherwise = -3/4*log(1-4*alpha/3)

evDistanceProtein :: Double -> Double
evDistanceProtein alpha
        | alpha >= 0.94 = 3.7
        | otherwise = -19/20*log(1-20*alpha/19)

-- Helper function that compares the sequences of 2 MolSeqs and returns a boolean list
-- True if both letters in the same position are equal. Else False. 
compareSeq :: String -> String -> [Bool]
compareSeq [] [] = []
compareSeq a b = (head a == head b) : compareSeq (tail a) (tail b)

-- Converts the boolean list to 1's and 0's. These are summed and divided by the length. 
-- Then subtracted from 1 to get the fraction of different positions. The result is the alpha value 
calculateAlpha :: [Bool] -> Int -> Double
calculateAlpha boolList a = 1-(fromIntegral(sum (map fromEnum boolList)) / fromIntegral a)

-- Measures the distance between two MolSeqs. The types must be equal and the distance formula depends on the type. 
-- First compare the sequences and get a boolean list. Then calculate the alpha value. Then calculate the distance.  
seqDistance :: MolSeq -> MolSeq -> Double 
seqDistance a b 
    | seqType a == seqType b && seqType a == DNA = evDistanceDNA (calculateAlpha (compareSeq (seqSequence a) (seqSequence b)) (seqLength b))
    | seqType a == seqType b && seqType a == Protein = evDistanceProtein (calculateAlpha (compareSeq (seqSequence a) (seqSequence b)) (seqLength b))
    | otherwise = error "Not same type"

-- Exercise 3.1
data Profile = Profile{
    nameOfProfile :: String,
    profileMatrix :: [[(Char,Int)]],
    profileType :: TypeOfSequence,
    numberOfSequences :: Int
} deriving (Show, Eq) 

-- Exercise 3.2
nucleotides = "ACGT"
aminoacids = sort "ARNDCEQGHILKMFPSTWYV"

-- Returns a profile matrix
makeProfileMatrix :: [MolSeq] -> [[(Char, Int)]]
makeProfileMatrix [] = error "Empty sequence list"
makeProfileMatrix sl = res
    where
        t = seqType (head sl)                                                       -- t is the profileType (DNA or Protein) 
        defaults =                      
            if (t == DNA) then
                zip nucleotides (replicate (length nucleotides) 0) -- Rad (i)       
                -- Nucleotide length is 4. We create an empty list [0,0,0,0]. Then we zip (combine into tuples) the nucleotide letters with each 0. 
                -- The result is [('A',0),('C',0),('G',0),('T',0)]
            else
                zip aminoacids (replicate (length aminoacids) 0) -- Rad (ii)
                -- Amino length is 20. Same process as above.  
                -- The result is [('A',0),('R',0),('N',0),('D',0),...,('V',0)]
        strs = map seqSequence sl -- Rad (iii)
                -- Strs is a list of all the sequences in the [MolSeq].
                -- Strs = [ACATAA, AAGTCA, ACGTGC]
        tmp1 = map (map (\x -> ((head x), (length x))) . group . sort)
                (transpose strs) -- Rad (iv)
                -- transpose strs = [AAA,CAC,AGG,TTT,ACG,AAC]
                -- Sort and group first. Then we create tuples with the letter and the number of 
                -- occurrences of that letter (in the "column"). 
                -- ["ACATAA", "AAGTCA", "ACGTGC"] -> [[('A',3)],[('A',1),('C',2)],[('A',1),('G',2)],[('T',3)],[('A',1),('C',1),('G',1)],[('A',2),('C',1)]]
        equalFst a b = (fst a) == (fst b) -- checks whether the letter in one tuple is the same as the letter in the empty DNA/protein tuple list 
        res = map sort (map (\l -> unionBy equalFst l defaults) tmp1)
                --  The last line will union (A,3) with (A,0), nothing with (C,0) etc. We get the result: 
                -- [[('A',3),('C',0),('G',0),('T',0)],[('A',1),('C',2),('G',0),('T',0)],...]

molseqs2profile :: String -> [MolSeq] -> Profile
molseqs2profile nameOfProfile seqList = Profile a b c d where 
            a = nameOfProfile
            b = makeProfileMatrix seqList
            c = seqType (head seqList)
            d = length seqList

-- Exercise 3.3
-- Returns the profile name
profileName :: Profile -> String 
profileName (Profile a _ _ _ ) = a

-- Returns the profile frequency. We get the column list for the specific index.  Then we get the specific element by selecting the character
-- Then we take the element's value and divide it by the number of sequences
profileFrequency :: Profile -> Int -> Char -> Double
profileFrequency p i c = fromIntegral(snd(head(filter (\(x,_)-> c==x) plist))) / fromIntegral(numberOfSequences p) where plist = profileMatrix p !! i

-- Exercise 3.4
-- Calculates the distance between two profiles. We create an operation where we caluclate the difference in relative frequency. 
-- We perform this operation on element by using two zipWiths (due to double array). We then take the sum of the matrix.   
profileDistance :: Profile -> Profile -> Double
profileDistance p q = sum(map sum matrix)
    where matrix = zipWith (zipWith difference) (profileMatrix p) (profileMatrix q) -- we perform a difference operation on each element (with relative numbers)
                where difference (_,x) (_,y) =  abs(divideHelper x (numberOfSequences p) - divideHelper y (numberOfSequences q))

divideHelper :: Int -> Int -> Double 
divideHelper a b = fromIntegral a / fromIntegral b

--Exercise 4.1 & 4.2
-- Evol data type
class Evol a where
    distance :: a -> a -> Double
    name :: a -> String
    distanceMatrix :: [a] -> [(String, String, Double)] 
    evolType :: a -> TypeOfSequence

    -- Calculates the distance between a list of Profiles or Molseqs. First we create a list of 3-tuples with all possible combinations. 
    -- Redundant pairs are then removed using nubBy. 
    distanceMatrix evolList = nubBy tupleEquality ([(name x, name y,distance x y) | x<-evolList, y<-evolList]) 
            where tupleEquality (a,b,_) (c,d,_) = (a == d) && (b == c)

instance Evol MolSeq where
    distance = seqDistance
    name = sequenceName
    evolType = sequenceType

instance Evol Profile where
    distance = profileDistance
    name = nameOfProfile
    evolType = profileType