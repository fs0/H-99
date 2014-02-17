-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt c xs 1 = c:xs
insertAt c (x:xs) p | p <= 0                = error "start at 1"
                    | p > (length $ x:xs)+1 = error "out of range"
                    | otherwise             = x : (insertAt c xs $ p-1)

-- Problem 22
range :: Int -> Int -> [Int]
range n m = [n..m]

--TODO (no System.Random)
-- Problem 23
-- Problem 24
-- Problem 25

-- Problem 26 TODO
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat $ map (put x) $ perms xs 
    where 
        put x [] = [[x]]
        put x (y:ys) = (x:y:ys) : (map (y:) $ put x ys)
