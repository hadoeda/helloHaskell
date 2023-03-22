module Hello 
    where

gh :: Int -> [Int]
gh a = map (+ a) [1,2,3]

ght :: Int -> [Int] -> [Int]
ght x = map (+ x)

sumr :: Int -> Int -> Int
sumr x = (+x)

fact :: Int -> Int
fact 0 = 1
fact x = x * fact(x - 1)

doubleVal :: Num a => a -> a
doubleVal x = x*2

doubleXY :: Num a => a -> a -> a
doubleXY x y = doubleVal x + doubleVal y

someConcat :: [a] -> [a] -> [a]
someConcat list1 list2 = list1 ++ list2

appendLeft :: a -> [a] -> [a]
appendLeft x list = x : list

appendLeft3 :: a -> a -> a -> [a] -> [a]
appendLeft3 x1 x2 x3 list = x1 :x2 : x3 : list

elementAt :: [a] -> Int -> a
elementAt list index = list !! index

constructArray :: (Num t, Enum t) => (t -> a) -> t -> (t -> Bool) -> [a]
constructArray elemFun count predicate = [elemFun x | x <- [1 .. count], predicate x]

getArray :: (Num a, Enum a) => a -> a -> [a]
getArray start count = constructArray (*start) count (\x -> True)

getFilteredArray :: (Num a, Enum a) => a -> a -> (a -> Bool) -> [a]
getFilteredArray start = constructArray (*start)

removeUpperCase :: [Char] -> [Char]
removeUpperCase str = [c | c <- str, c `elem` ['a' .. 'z']]

someHead :: [a] -> a
someHead (x : _) = x

addVectors2 :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

middle :: [a] -> a
middle [] = error "cant"
middle arr = let l = length arr in 
    arr !! ((l `div` 2 + l `mod` 2) - 1)

duator :: Ord a => [a] -> ([a], [a])
duator arr
    | l == 0 = ([], [])
    | otherwise = ([a | a <- arr, a < mid], [b | b <- arr, b >= mid])
    where 
        l = length arr
        mid = arr !! ((l `div` 2 + l `mod` 2) - 1)  
 
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort[a | a <- xs, a <= x] ++ [x] ++ qsort [b | b <- xs, b > x]

someZip :: [a] -> [b] -> [(a, b)]
someZip _ [] = []
someZip [] _ = []
someZip (a:ax) (b:bx) = (a, b) : someZip ax bx

someReverse :: [a] -> [a]
someReverse [] = []
someReverse (x : xs) = someReverse xs ++ [x]

someZipWith :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
someZipWith _ _ [] = []
someZipWith _ [] _ = []
someZipWith f (a:ax) (b:bx) = f a b : someZipWith f ax bx

someZipp :: [t1] -> [t2] -> [(t1, t2)]
someZipp = someZipWith (,) 

someMap :: (t -> a) -> [t] -> [a]
someMap _ [] = []
someMap f (x:xs) = f x : someMap f xs 

someMapp :: (t -> a) -> [t] -> [a]
someMapp _ [] = []
someMapp f arr = [f x | x <- arr]

someFilter :: (a -> Bool) -> [a] -> [a]
someFilter _ [] = []
someFilter p (x:xz)
    | p x = x : someFilter p xz
    | otherwise = someFilter p xz

someFilterr :: (a -> Bool) -> [a] -> [a]
someFilterr _ [] = []
someFilterr p arr = [a | a <- arr, p a]

someFold :: (a -> t -> a) -> [t] -> a -> a
someFold _ [] acc = acc
someFold f (x : xs) acc = someFold f xs (f acc x) 

someScan :: (a -> t -> a) -> [t] -> a -> [a]
someScan _ [] acc = [acc]
someScan f (x : xs) acc = acc : someScan f xs (f acc x)

someCompose :: (a -> b) -> (b -> c) -> a -> c
someCompose f1 f2 = f2 . f1

someNegate :: [[Integer]] -> [Integer]
someNegate = map (negate . sum . tail)

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

main :: IO ()
main = do
    print "Hello world!"