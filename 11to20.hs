data Number a = Single a | Multiple Int a deriving Show

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : (takeWhile (==x) xs)) : pack (dropWhile (==x) xs)

encodeModified :: (Eq a) => [a] -> [Number a]
--encodeModified = map (\x -> if (length x) == 1 then (Single (x!!0)) else (Multiple (length x) (x!!0))) . pack
encodeModified = map checkNumber . map (\x -> (length x, x!!0)) . pack
    where
        checkNumber (1,c) = Single c
        checkNumber (x,c) = Multiple x c

decodeModified :: [Number a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = (getNumber x) ++ decodeModified xs
    where
        getNumber (Single x) = [x]
        getNumber (Multiple x y) = [ y | _ <- [1..x] ]

encodeDirect :: (Eq a) => [a] -> [Number a]
encodeDirect [] = []
encodeDirect (x:xs) = (makeNumber (x:(takeWhile (==x) xs))) : encodeDirect (dropWhile (==x) xs)
    where
        makeNumber [x] = Single x
        makeNumber xs = Multiple (length xs) (xs!!0)

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n
-- pointfree
repli' :: [a] -> Int -> [a]
repli' = flip $ concatMap . replicate

dropEvery :: [a] -> Int -> [a]
dropEvery [] n = []
dropEvery xs n = take (n-1) xs ++ dropEvery (drop n xs) n

split :: [a] -> Int -> [[a]]
split xs n = take' n xs : [drop' n xs]
    where
        take' _ [] = []
        take' 0 _ = []
        take' n (x:xs) = x : take' (n-1) xs
        drop' _ [] = []
        drop' 0 xs = xs
        drop' n (x:xs) = drop' (n-1) xs

slice :: [a] -> Int -> Int -> [a]
slice xs i j = take (j-i+1) $ drop (i-1) xs

rotate :: [a] -> Int -> [a]
rotate xs n = if n < 0 then
                (drop shiftNeg xs) ++ (take shiftNeg xs)
              else
                (drop  shiftPos xs) ++ (take shiftPos xs)
              where
                len = length xs
                shiftNeg = mod (len + n) len
                shiftPos = mod n len

removeAt :: Int -> [a] -> (a, [a])
--removeAt 1 (x:xs) = (x,xs)
--removeAt pos (x:xs) = (((x:xs)!!(pos-1)), [x] ++ snd ( removeAt (pos-1) xs)))
removeAt pos xs = (xs!!(pos-1), take (pos-1) xs ++ drop pos xs)
