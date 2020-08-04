doubleMe :: Int -> Int
doubleMe x = x + x

doubleUs :: Int -> Int -> Int
doubleUs x y = x * 2 + y * 2

doubleSmallNumber :: Int -> Int
doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

lucky :: (Eq a, Num a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you’re out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x 
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | a <= b = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight."
  | bmi <= normal = "You're supposedly normal."
  | bmi <= fat = "You're fat!"
  | otherwise = "Youre a whale."
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
  where what [] = "empty."
        what [x] = "a singleton list."
        what xs = "a longer list."

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs

replicate' :: (Integral a) => a -> b -> [b]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n - 1) x

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

-- applyTwice (+3) 10
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- find the largest number under 100,000 that’s divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3892 == 0

-- find the sum of all odd squares that are smaller tahn 10000
sumOddSquares :: (Integral a) => a
sumOddSquares = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

hasTwoCubeSums :: Integer -> Bool
hasTwoCubeSums n
  | n < 2 = False
  | otherwise = twoCubeSums n (takeWhile (\i -> i^3 < n) [1..]) 0

twoCubeSums :: Integer -> [Integer] -> Integer -> Bool
twoCubeSums _ _ 2 = True
twoCubeSums _ [x] _ = False
twoCubeSums n (x:ys) count
  | not (null [x | a <- ys, x^3 + a^3 == n]) = twoCubeSums n ys (count + 1)
  | otherwise = twoCubeSums n ys count
