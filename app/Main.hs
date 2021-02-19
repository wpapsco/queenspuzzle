module Main where
import Control.Monad
import Data.List
import Data.Functor
import Control.Monad.Random
import Data.Maybe
import Lib

type Pos = (Int, Int)
type Gene = [Int]

data Params = Params {popSize :: Int, children :: Int, mutateRate :: Float}

-- Determine if two queens see each other.
queensSee :: Pos -> Pos -> Bool 
queensSee (x1, y1) (x2, y2) = not same && (sameRank || sameFile || sameDiag)
    where same = x1 == x2 && y1 == y2
          sameRank = y1 == y2
          sameFile = x1 == x2
          sameDiag = abs (x2-x1) == abs (y2-y1)

-- A list of positions for queens is good if none of the queens see any of the
-- rest of the queens.
isGood :: [Pos] -> Bool
isGood [] = True
isGood (p:rest) = not (any (queensSee p) rest) && isGood rest

-- Count the number of clashes between one queen and the rest of the queens.
-- For the purposes of this function, queens have x-ray vision.
countClashes :: [Pos] -> Pos -> Int
countClashes xs x = length $ filter (queensSee x) xs

-- Determine fitness by counting the total number of queen clashes and
-- subtracting from the maximum number of clashes; 28.
-- Higher fitness = less clashes.
fitness :: Gene -> Int
fitness gene = 28 - (`div` 2) (sum $ map (countClashes board) board)
    where board = geneToPos gene

-- Get the average fitness of a generation of genes.
getAvgFitness :: [Gene] -> Float
getAvgFitness xs = sum fs / realToFrac (length xs)
    where fs = map (realToFrac . fitness) xs

-- Convenience function to convert from genes to positions.
geneToPos :: Gene -> [Pos]
geneToPos = zip [0..]

-- Shamelessly stolen from
-- https://hackage.haskell.org/package/monad-extras-0.6.0/docs/Control-Monad-Extra.html#v:iterateM
-- It was either that or download the entire library. I think I made a good 
-- choice.
iterateM :: Monad m => (a -> m a) -> a -> m [a]
iterateM f x = do
    x' <- f x
    (x':) `fmap` iterateM f x'

-- It all starts here; simply generate a random gene.
genGene :: RandomGen g => Rand g Gene
genGene = replicateM 8 (getRandomR (0, 7))

-- This function is kind of a mess but it basically randomly selects n elements
-- of vals weighted by the second element of the tuple. Uses the "linear scan"
-- algorithm. (https://blog.bruce-hill.com/a-faster-weighted-random-choice)
select :: RandomGen g => Int -> [(a, Float)] -> Rand g [a]
select n vals = map get <$> replicateM n (getRandomR (0, m))
    where xs = zip (map fst vals) (tail (scanl (+) 0 (map snd vals)))
          m = snd $ last xs
          get x = case find ((x <=) . snd) xs of 
                  (Just (z, _)) -> z 
                  Nothing -> error "get out of range"

-- Randomly select and pair up parents weighted by their fitness. Generates the
-- correct number of parents such that the population size is maintained.
selectParents :: RandomGen g => Params -> [Gene] -> Rand g [(Gene, Gene)]
selectParents ps pool = uncurry zip . splitAt (div numParents 2) <$> select numParents withFit
    where withFit = zip pool (map (realToFrac . fitness) pool)
          numParents = div (popSize ps) (children ps) * 2

-- Take the first i elements of a and the remaining elements of b and just jam
-- them together.
splice :: (Gene, Gene) -> Int -> Gene
splice (a, b) i = x ++ y
    where (x, _) = splitAt i a
          (_, y) = splitAt i b

-- Breed two parents by randomly splicing them together.
breed :: RandomGen g => Params -> (Gene, Gene) -> Rand g [Gene]
breed ps parents = replicateM (children ps) $ splice parents <$> getRandomR (0, 8)

-- Replace element i in the list with x.
replace :: Int -> a -> [a] -> [a]
replace i x = zipWith (\j e -> if j == i then x else e) [0..]

-- Take a gene and mutate it by randomly replacing one element with another one.
mutate :: RandomGen g => Gene -> Rand g Gene
mutate gene = liftM3 replace (getRandomR (0, 7)) (getRandomR (0, 7)) (return gene)

-- Either returns a with probability p, or return b with probability 1-p
thisOr :: RandomGen g => Float -> a -> a -> Rand g a
thisOr p a b = (\x -> if x <= p then a else b) <$> getRandomR (0.0, 1.0)

-- Take a gene and maybe mutate it or maybe don't mutate it based on the
-- mutation rate.
maybeMutate :: RandomGen g => Params -> Gene -> Rand g Gene
maybeMutate ps gene = mutate gene >>= thisOr (mutateRate ps) gene

-- Take a list of genes and breed & mutate them all together to produce a new
-- list of genes. Represents doing one generation of the algorithm.
-- This is probably my favorite function in this program. I mean just look at it
-- it's wonderful!
generation :: RandomGen g => Params -> [Gene] -> Rand g [Gene]
generation ps genes = selectParents ps genes >>= mapM (breed ps) >>= mapM (maybeMutate ps) . concat

-- Generate an infinite list of generations. Just keeps running the genetic 
-- algorithm generation after generation. Uses interleave to split the PRNG so 
-- it doesn't just run forever when used in sequence with other calculations.
genList :: RandomGen g => Params -> Rand g [[Gene]]
genList ps = interleave $ replicateM (popSize ps) genGene >>= iterateM (generation ps)

-- Generates a list of fitness values for each generation. Averages the fitness
-- of each generation and accumulates it in a list.
avgFitness :: RandomGen g => Int -> Params -> Rand g [Float]
avgFitness n ps = take n . map getAvgFitness <$> genList ps

-- Runs the genetic algorithm until it finds a solution that works and returns 
-- it along with its generation number.
-- Not the most efficient solution I'm sure but I'm not too concerned about it.
findSolution :: RandomGen g => Params -> Rand g (Int, Gene)
findSolution ps = (\(n, xs) -> (n, fromMaybe [] (find (isGood . geneToPos) xs))) <$> gs
    where gs = fromMaybe (0, []) . find (any (isGood . geneToPos) . snd) . zip [1..] <$> genList ps -- :: Rand g (Int, [Gene]) (one whole generation that includes a solution to the puzzle and its number)

tests :: [Params]
tests = [
    Params 100 2 (1 - 0.1),
    Params 1000 2 (1 - 0.1),
    Params 10000 2 (1 - 0.1),
    Params 100 2 (1 - 0.01),
    Params 1000 2 (1 - 0.01),
    Params 10000 2 (1 - 0.01),
    Params 100 2 (1 - 0.001),
    Params 1000 2 (1 - 0.001),
    Params 10000 2 (1 - 0.001)]

testAll :: RandomGen g => Rand g [[Float]]
testAll = mapM (avgFitness 1000) tests

main :: IO ()
main = newStdGen >>= mapM_ print . evalRand testAll