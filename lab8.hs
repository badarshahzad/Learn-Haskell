a = ["  aaaaa  "," a     a "," a     a "," aaaaaaa "," a     a "," a     a "," a     a "] 

--badar = putStr( concat (map (++ "\n")(letter 'a')))

--showMat ::  Char -> IO()
--showMat ch = putStr (concat ( map(++ "\n") (letter 'a')))


replicateIt :: Int -> [Char] -> [Char]
replicateIt x ls=take x (cycle ls)


--repeatIt :: Int -> [Char] -> [[Char]]
repeatIt num []=[]
repeatIt num (x:sx)= replicateIt num [x]:(repeatIt num sx)

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

showMat' :: [String] -> IO()
showMat' strList = putStr (concat  (stretch 1 1 strList))

--Left Attach Character
leftattach :: Char -> [String] -> [String]
leftattach a strlist =  map ( a: ) strlist 

showMatCharAttachLeft :: Char -> [String] -> IO()
showMatCharAttachLeft a strList = putStr (concat  (stretch 1 1 ( leftattach a strList)))


charToString :: Char -> String
charToString a = a:[]

--Right Attach Character
rightattach :: Char -> [String] -> [String]
rightattach a strlist = map (++(charToString a)) strlist

showMatCharAttachRight :: Char -> [String] -> IO()
showMatCharAttachRight a strList = putStr (concat  (stretch 1 1 ( rightattach a strList)))





--allLeft 

--appendLeft :: Char -> [String] -> String
--appendLeft 'a' [] = []
--appendLeft 'a' strlist =  concat ( 'a':leftattach strlist);


delete a st = drop a st

--star :: Int -> String -> [String]
star a st = take a st

--Give a number and string and result will be in list of string

toStringList :: Int -> String -> [String]
toStringList i [] = []
toStringList i str = (take i str): (toStringList i(drop i str))

stich :: Int -> [String] -> String -> [String]
stich num strlist str =  zipWith (++) strlist (toStringList (num - length(head strlist)) str)








