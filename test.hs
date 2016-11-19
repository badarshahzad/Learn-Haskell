charName ::  Char -> String
charName 'a' = "Wa lala wa"
charName 'b' = "Zabar dast"
charName 'c' = "Chal Hunr Dafa ho"
charName a = error  "Dafa ho sahi input da! :/ "


map' f [] = []
map' f xs = [f x | x <- xs]

fil' :: (a->Bool) -> [a] -> [a]
fil' f [] = []
fil' f xs = [x | x <- xs, f x]

fil'' p[] = []
fil'' p (x:xs) | p x = x:fil'' p xs
               | otherwise = fil'' p xs

len :: [a]-> Int
len [] = 0
len (x:xs) = 1 + len xs
