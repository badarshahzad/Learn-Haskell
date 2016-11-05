
--Here is the quick sort
count :: a -> Int -> Int
count x y = 1+y;




{-
filter'::(a->Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
 | f x = x: filter' f(xs)
 |otherwise = filter' f x

-}

{-
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

--zip :: [a] -> [b] -> [(a,b)]
--zip (a:as) (b:bs) = (a,b)
--zip _      _      = []
-}

inc x = x+1

incx x y = (x+1)*y

inclist [] = []
inclist (x:xs) = inc x: inclist(xs)

multiply2 [] = []
multiply2 (x:xs) = x*2:multiply2 xs;

{-
--inclistMultiply [] _ = []
--inclistMultiply y  (x:xs) = incx x y : inclistMultiply xs
-}

zipwith' _ [] = []
zipwith' f (x:xs)= f x : zipwith' f (xs)
--zipwith1 inc (muliply2 (x:xs)) = inc (multiply2 x): zipwith' f (xs)

{-
zipwithfilter::(Bool -> Bool) -> [a] -> Bool 
zipwithfilter f (x:xs) = ( if (f (x) > xs == True) then zipwithfilter f (xs) else False
-}

{- 
zipwithlist _ _ [] = []
zipwithlist _ [] _ = []
zipwithlist f (x:xs) (y:ys) = f x : f y : zipwithlist f (xs) (ys)
-}

{-
findlargest f (x:xs)
 | f x =  x/3829==0
 | otherwise = findlargest f (xs) 
-}
