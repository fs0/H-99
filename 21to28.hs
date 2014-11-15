import System.Random


-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt c xs 1 = c:xs
insertAt c (x:xs) p | p <= 0                = error "start at 1"
                    | p > (length $ x:xs)+1 = error "out of range"
                    | otherwise             = x : (insertAt c xs $ p-1)

-- Problem 22
range :: Int -> Int -> [Int]
range n m = [n..m]


-- haskell-tf-random
-- http://hackage.haskell.org/package/tf-random

-- Problem 23

rnd_select :: [a] -> Integer -> IO [a]
rnd_select xs 1 = randomElement xs
rnd_select xs n
    | n < 0 = error "invalid number"
    | otherwise = do
        l <- randomElement xs
        b <- rnd_select xs (n-1)
        return $ l++b

randomElement :: [a] -> IO [a]
randomElement [] = error "empty list"
randomElement xs = do
    i <- randomRIO (0,(length xs)-1)
    return $ [xs!!i]

-- Problem 24
-- Problem 25

-- Problem 26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)

-- Problem 28 a)
lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (xs:xss) = s ++ [xs] ++ b
    where
        s = lsort [ a | a <- xss, (length a) <= (length xs) ]
        b = lsort [ a | a <- xss, (length a) >  (length xs) ]
