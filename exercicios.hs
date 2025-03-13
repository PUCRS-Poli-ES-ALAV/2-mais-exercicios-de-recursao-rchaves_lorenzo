fat :: Int -> Int
fat 0 = 1
fat 1 = 1
fat n = n * (fat (n - 1))

sm :: Int -> Int
sm 0 = 0
sm n = n + (sm (n - 1))

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = (fib (n - 1)) + (fib (n - 2))

sumRange :: Int -> Int -> Int
sumRange 0 0 = 0
sumRange k j = if k > j then 0 else j + (sumRange k (j - 1))

reversee :: String -> String
reversee [] = []
reversee (x : xs) = reverse xs ++ [x]

isPal :: String -> Bool
isPal s = s == (reversee s)

convBase2 :: Int -> String
convBase2 0 = "0"
convBase2 1 = "1"
convBase2 n =
  if (n `mod` 2) == 1
  then convBase2 (n `div` 2) ++ "1" 
  else convBase2 (n `div` 2) ++ "0"

-- 9 mod 2 => 1
-- 9 div 2 => 4
--
-- 9 => 1001

sumList :: [Int] -> Int
sumList [] = 0
sumList (x : xs) = x + (sumList xs)

maxList :: [Int] -> Int -> Int
maxList [] acc = acc
maxList (x : xs) acc = if x > acc then maxList xs x else maxList xs acc

findSubstr :: String -> String -> Bool
findSubstr []      _     = False
findSubstr _      []     = True
findSubstr (x:xs) (y:ys) = if x == y then findSubstr xs ys else findSubstr xs (y:ys)

nroDigit :: Int -> Int
nroDigit n = if n < 10 then 1 else 1 + nroDigit (n `div` 10)

permutations :: String -> [String]
permutations str = 
 if length str == 0 then [""]
 else concat [map (x:) (permutations (delete x str)) | x <- str]
 where
   delete x [] = []
   delete x (y:ys) = if x == y then ys else y : delete x ys

main :: IO ()
-- main = print $ show (convBase2 16)
main = print $ (permutations "aab")
-- main = print $ show (9 `mod` 2)

