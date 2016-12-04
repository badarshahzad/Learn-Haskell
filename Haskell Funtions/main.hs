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

str = "My long \n\
\ long"

funChar :: Char -> Char
funChar a
     | isLower(a)==True = ' '
     | isUpper(a)==True = ' '
     | otherwise = '@'

changer :: [Char] -> [Char] -> [Char]
changer [] res = res
changer (x:xs) res = changer xs (res ++ (if x == ' ' then "%20" else [x]))

sanitize :: [Char] -> [Char]
sanitize xs = changer xs ""


blackToWhiteAString :: [Char] -> [Char]
blackToWhiteAString bs = [a | b <- bs, a <- 
                if (b == ' ')
                   then "X"
                 else if (isLower(b)==True) 
                   then " " 
                 else if (isUpper(b)==True)
                   then " "
                   else [b]]


compPic :: [String] -> [String]
compPic  [] = []
compPic  (a:as) = map (a)

showMatBlackToWhite :: [String] -> IO()
showMatBlackToWhite strList = putStr (concat  (stretch 1 1 (compPic (strList)) ))

--Length of String
len s = length s;

--List of String length sum
str :: [String] -> Int
str a =  sum (map len a)



hStretchChar :: Int  -> Char  -> String 
hStretchChar i ch = replicate i ch

hStretchString :: Int -> String -> String 
hStretchString i sts = concat ( map ( hStretchChar i) sts)

hStretchListOfString :: Int -> [String] -> [String]
hStretchListOfString i stlist = map (hStretchString i ) stlist

vStretchString :: Int -> String -> String
vStretchString i str = concat (replicate i (str ++ "\n"))

vStretchListOfString :: Int -> [String] -> [String]
vStretchListOfString  i strList = map (vStretchString i) strList


stretch :: Int -> Int -> [String] -> [String]
stretch i j strList = vStretchListOfString i (hStretchListOfString  j strList)

showPic :: BPic -> IO()
showPic strList = putStr (concat  (stretch 1 1 strList))
--showPic strList = putStr (concat  (map (++"\n") strList))

listofNumbers a = listofNumbersEqual (length a) a

listofNumbersEqual x a
        | (head a)== a!!(x-1) = True
	| otherwise = False

isEqual :: [Int] -> Bool
isEqual a = all (==(head a)) a

isEqual' :: [Int] -> Bool
isEqual' [] = True
isEqual' (x:xs) = not $ any (/= x) xs



f (a:as) (b:bs) = [a | a <- as, b <- bs,x <- 
                 if (a == ' ' || b== ' ')
                   then a:[]
                 else if  (a == 'X' || b== ' ')
                   then a:[]
                 else if  (a == 'X' || b== 'X')
                   then a:[] 
                 else if  (a == ' ' || b== 'X') 
                   then a:[] 
                 else []]

aa (a:as) (b:bs)= [f a b | a <- as, b <- bs]

shiftPic :: Int -> Int -> BPic -> BPic
shiftPic num1 num2 list
                       | num2<=0 = iterate rolldown list !! abs(num2)
                       | num2>=0 = iterate rollup list !! num2 
                       | num1<=0 = iterate moveleft list !! abs (num1)

frames :: String -> Char -> String
frames [] b = []
frames (a:as) b =  ([b] ++ [a] ++ [b]) ++ frames as b
