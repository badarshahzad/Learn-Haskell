sumlist [] = 0
sumlist (x:xs) = x +  (sumlist xs)

productlist [] = []
productlist (x:xs) = x*2 : (productlist xs)
{-
maxint (x:xs) = where maxnum = maxint [a|a <- xs,x>=xs]


foldr':: [Int] -> Int
foldr' f [] = 1
foldr' f (x:xs) = f x + foldr' f (xs)
-}

---ThisFuncitonISUsedToMakeListThatMuliply---
zipwith' f [] _ = []
zipwith' f _ [] = []
zipwith' f xs ys = [ f x y | x <- xs, y <- ys]

--Hasi--
factorial a = foldl (*) 1 [1..a]


rever (x:xs) = foldr (head) (length(xs)-1) xs

