myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (x:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "not in range"
elementAt (x:xs) 1 = x
elementAt (x:xs) i = elementAt xs (i-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = myReverse xs == xs

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ (flatten (List xs))

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:(y:xs)) = if (x==y) then compress (x:xs) else x:(compress (y:xs))

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : (takeWhile (==x) xs)) : pack (dropWhile (==x) xs)
--pack = foldr f []
--  where f x [] = [[x]]
--        f x (y:ys) = if (x == y!!0) then (x:y):ys else [x]:(y:ys)

encode :: (Eq a) => [a] -> [(Int,a)]
encode = map (\x -> (length x, x!!0)) . pack
