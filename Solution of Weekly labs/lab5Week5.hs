--Week 5 Task 1

{-
fac:: Int -> Int
fac 0 = 1
fac 1 = 1
fac a = a * fac(a-1) 
-}
fac'::Int -> Int
fac' x
	| x > 1 = x*fac'(x-1)
	|otherwise	=1

max' a b
	| a>b	=a
	| otherwise	=b

compare'::(Ord a) => a -> a-> Ordering
compare' x y
	| x<y	=LT
	| x>y	=GT
	|otherwise =EQ


(///) x y = x / y

isLower:: Char -> Bool
isLower x = 'a' <=x && x<= 'z'

isCapital:: Char -> Bool
isCapital x = 'A' <=x && x<= 'Z' 

isDigit:: Char -> Bool
isDigit x = '0' <= x && x<= '9'

isChar :: Char -> Bool
isChar x = isLower x || isCapital x || isDigit x

{-
toUpper :: Char -> Char
toUpper c
 | isLower c = "chr" ("ord" c - "ord" 'a' + "ord" 'A')
 | otherwise = c
-}

wcal w h
 | m <=30 = "Alla"
 | m <=60 = "Nice"
 | m <=90 = "Haya Kar"
 | otherwise = "I don't believe"
 where m= w/h

maxOfThree'::Int->Int->Int->Int
maxOfThree' a b c 
 | (a>=b && b>=c) = a 
 | b>=c           = b 
 | otherwise      = c

enumFromthis x = x : enumFromthis (x+1)

--Task 2
sumfactorial x
 | x==0 =1
 | otherwise = fac(x)+ sumfactorial(x-1)

fac x
 | x==0 =1
 | x==1 =1
 | otherwise = x*fac(x-1)

--Task 3
remainder x y =
 if x>=y
  then remainder (x-y) y
 else x

divider::Int->Int->Int 
divider m n 
 | m<n      = 0
 |otherwise = 1 + divider (m-n) n


(^^^):: Int -> Int -> Int
x^^^1= x
x^^^y= x*(x^^^(y-1))

init':: [a] -> [a]
init' (x:xs) = take (length (x:xs)-1) (x:xs)

----(a)init----
init''::[a]->[a]
init'' (x:[]) = []
init'' (x:xs)   = x:init'' xs

replicate'::Int -> a -> [a]
replicate' x y = take x (cycle[y])
---MyreplicateFailOnNumbersAndStrings---

----(b)replicate----
replicate''::Int->a->[a]
replicate'' 0 x = []
replicate'' n x = x:replicate'' (n-1) x

zip':: [a] -> [b] ->[(a,b)]
zip' [] _ = []
zip' _ [] =[]
zip' (x:xs) (y:ys) = (x,y): zip' (xs) (ys)

zip'' :: [a] -> [b] -> [(a,b)]
zip'' [] ys = []
zip'' xs [] = []
zip'' (x:xs) (y:ys) = (x,y) : zip'' xs ys

pairs:: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

pairs' :: [a] -> [(a,a)]
pairs' xs = zip xs (tail xs)

zip3'::[a]->[b]->[c]->[(a,b,c)]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z):zip3' xs ys zs


zip3'':: [a]-> [a] -> [a]->[(a,a,a)]
zip3'' _ [] _ = []
zip3'' [] _ _ = []
zip3'' _ _ [] = [] 
zip3'' (x:xs) (y:ys) (z:zs) = (x,y,z):zip3'' xs ys zs

{-
dotproduct:: 
dotproduct [] [] = ()
dotproduct (x:xs) (y:ys) = (x.y) : dotproduct (xs) (ys)
-}
------- Using List Comprehension
dot :: Num a => [a] -> [a] -> a
dot xs ys = sum [ x*y | (x,y) <- zip' xs ys ]

------- Recursive ------------

dotRec :: Num a => [a] -> [a] -> a
dotRec [] [] = 0
dotRec (x:xs) (y:ys) = x*y + dotRec xs ys


--What I will do for False,False,False condition
myor::[Bool] -> Bool
myor (x:xs)
 | x==True =True
 | otherwise = myor xs
---------------Q 13----------------

myOR::[Bool]->Bool
myOR [] = False
myOR (x:xs) = x || myOR xs


---------------Q 14----------------
---------- using let-----------	
qsort :: (Ord a) => [a] -> [a]  
qsort [] = []  
qsort (x:xs) =   
    let smallerSorted = qsort [a | a <- xs, a <= x]  
        biggerSorted = qsort [a | a <- xs, a > x]  
    in  biggerSorted ++ [x] ++ smallerSorted

---------- using where -----------	
qsort' :: (Ord a) => [a] -> [a]  
qsort' [] = []  
qsort' (x:xs) = smallerSorted ++ [x] ++ biggerSorted 
    where smallerSorted = qsort [a | a <- xs, a <= x]  
          biggerSorted = qsort [a | a <- xs, a > x]  

halve::[a]->([a],[a])
halve xs = (take (n`div`2) xs, drop (n`div`2) xs)
           where n = length xs

merge::Ord a=> [a]->[a]->[a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) 
 | x<y      = x:merge xs (y:ys)
 |otherwise = y:merge (x:xs) ys  

merge'::Ord a=> [a] -> [a] -> [a]
merge' [] xs = xs
merge' xs [] = xs
merge' (x:xs) (y:ys)
 | x<y = x:merge' xs (y:ys)
 | otherwise = y:merge' (x:xs) ys

msort::Ord a => [a]->[a]
msort xs 
 |(length xs)>1 = merge (msort ls) (msort rs)
 | otherwise    = xs
 where (ls,rs) = halve xs


insert::Int->[Int]->[Int]
insert x  []    = [x]
insert x (y:ys)  
 | x<y      = x:y:ys
 |otherwise = y: insert x ys
 
isort::[Int]->[Int]
isort []     = []
isort (x:xs) = insert x (isort xs)

{-
take' a (x:xs) = take' [x <-xs,x<=a]

take'' 0 [] = []
take'' a xs = [a !! k | k <- take'' a-1 xs]
-}

foo xs = [(length xs-1), (length xs-2)..0]
rev (xs) = [xs !! k | k <- foo xs]

rev' [] = []
rev' xs = last xs : rev' (init xs)









