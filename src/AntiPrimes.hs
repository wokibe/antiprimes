-- | a.k.a. highliy composite numbers
module AntiPrimes
       (Nui, Siz, Lst, size, dividers, proof, list) where

import Data.List (foldr1, group, nub, sort, tails)
import Data.Numbers.Primes (primeFactors)

-- | Number under investigation
type Nui = Int
-- | number of divisors of Nui
type Siz = Int
-- | list of tuples: numbers and their divisor sizes
type Lst = [(Nui, Siz)]

-- calculate the frequencies of the list elements
frequ :: [Int] -> [(Int, Int)]
frequ list = map (\l -> (head l, length l)) (group (sort list))

-- good old combinatorics: combine n elements of a list without repetitions
comb :: (Eq a, Num a) => a -> [a1] -> [[a1]]
comb 0 lst = [[]]
comb n lst = do
  (x:xs) <- tails lst
  rest <- comb (n-1) xs
  return $ x:rest

-- all possible combinations of a list
combAll :: (Eq a, Num a) => a -> [a1] -> [[a1]]
combAll 0 lst = []
combAll n lst = (comb n lst) ++ (combAll (n-1) lst)

-- error message
msg :: String
msg = "the number must be greater than 0"

lst1 :: Lst
lst1 = [(1,1)]

-- multiplication all elements of a list
mul :: (Num a, Foldable t) => t a -> a
mul x = foldr1 (*) x

-- generate a tuple with a number and ist size
tup :: Nui -> (Nui, Siz)
tup n = (n, size n)

-- list of (n, size) for n = [2..n]
lst :: Nui -> Lst
lst n
  | n <= 0  = error msg
  | n == 1  = lst1
  | otherwise = map tup rng
                where rng = [2..n]  -- range of interest

-- | calculate the "size" of all possible factors of a number
--
-- >>> import qualified AntiPrimes as AP
-- >>> AP.size 12
-- 6                -- as [1,2,3,4,6,12] are possible
--
size :: Nui -> Siz
size n
  | n <= 0    = error msg
  | n == 1    = 1
  | otherwise = foldr1 (*) incrs
                where facts = primeFactors n
                      frequs = frequ facts
                      counts = map snd frequs
                      incrs  = map (1 +) counts

-- | calculate all possible dividers of a number
--
-- >>> import qualified AntiPrimes as AP
-- >>> AP.dividers 12
-- [1,2,3,4,6,12]
--
dividers :: Nui -> [Int]
dividers n
  | n <= 0    = error msg
  | n == 1    = [1]
  | otherwise = sort add1
                where facts = primeFactors n
                      len   = length facts
                      combs = combAll len facts
                      multi = map mul combs
                      clean = nub multi
                      add1  = clean ++ [1]

-- | proof if a number is an antiprime
--
-- >>> import qualified AntiPrimes as AP
-- >>> AP.proof 4
-- True
-- >>> AP.proof 5
-- False
--
proof :: Nui -> Bool
proof n
  | n <= 0    = error msg
  | n == 1    = True
  | n == 2    = True
  | otherwise = length flt == 1
                where siz = size n
                      flt = filter (\(a, b) -> b >= siz) $ lst n

-- | show all (antiprimes, size) below a number
--
-- >>> import qualified AntiPrimes as AP
-- >>> AP.list 12
-- [(1,1),(2,2),(4,3),(6,4)]
--
list :: Int -> Lst
list 0 = error msg
list 1 = lst1
list n
  | n <= 0    = error msg
  | n == 1    = lst1
  | otherwise = list' lst1 (lst n)

-- helper function to clean out excluded entries
list' :: Lst -> Lst -> Lst
list' aps [] = aps
list' aps rest = list' nxt est
  where top = head rest
        rst = tail rest
        nxt = aps ++ [top]
        est = filter (\(a, b) -> b > (snd top)) $ rst
