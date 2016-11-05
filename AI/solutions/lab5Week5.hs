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

(^^^):: Int -> Int -> Int
x^^^y
 | z<=y = z+1
 | otherwise = x*x
 where z=0

init':: [a] -> [a]
init' (x:xs) = take (length (x:xs)-1) (x:xs)

replicate' x y = take x (cycle[y])

zip':: [a] -> [b] ->[(a,b)]
zip' [] [] = []
zip' [] _ = []
zip' _ [] =[]
zip' (x:xs) (y:ys) = (x,y): zip' (xs) (ys)


pairs xs = zip xs (tail xs)

{-
zip3 [] [] [] = []
zip3 _ [] [] = []
zip3 [] _ [] = []
zip3 [] [] _ = [] 
zip3 (x:xs) (y:ys) (z:zs) = (x,y,z): zip3 (xs) (ys) (zs)



--dotproduct [] [] = ()
--dotproduct (x:xs) (y:ys) = (x.y) : dotproduct (xs) (ys)

--What I will do for False,False,False condition
myor::[Bool] -> Bool
myor (x:xs)
 | x==True =True
 | otherwise = myor xs


take' a (x:xs) = take' [x <-xs,x<=a]

take'' 0 [] = []
take'' a xs = [a !! k | k <- take'' a-1 xs]
-}

foo xs = [(length xs-1), (length xs-2)..0]
rev (xs) = [xs !! k | k <- foo xs]

rev' [] = []
rev' xs = last xs : rev' (init xs)








