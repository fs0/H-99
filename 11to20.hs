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
