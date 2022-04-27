-- my haskell cheatsheet
-- list integer biasa

intList1 :: [Int]
intList1 = [1,2,3,4,5]

-- fungsi dasar (cases)

sumIntList1 :: [Int] -> Int
sumIntList1 []       = 0
sumIntList1 (x:xs)   = x + sumIntList1 xs

lengthIntList1 :: [Int] -> Int
lengthIntList1 []       = 0
lengthIntList1 (x:xs)   = 1 + lengthIntList1 xs

-- hanya bisa partial function
takeIntList1 :: Int -> [Int] -> [Int]
takeIntList1 0 _             = []
takeIntList1 a (x:xs)        = x : (takeIntList1 (a-1) xs)

-- fungsi dasar (guards)

sumIntList2 :: [Int] -> Int
sumIntList2 []           = 0
sumIntList2 (x:xs)
     | length (x:xs) > 0 = x + sumIntList2 xs
     | otherwise         = 0

lengthIntList2 :: [Int] -> Int
lengthIntList2 []     = 0
lengthIntList2 (x:xs)
     | length (x:xs) > 0 = 1 + lengthIntList2 xs
     | otherwise         = 0

takeIntList2 :: Int -> [Int] -> [Int]
takeIntList2 a (x:xs)        
     | a <= 0                = []
     | a > (length (x:xs))   = (x:xs)
     | otherwise             = x : (takeIntList2 (a-1) xs)

-- abstract data types
data Person = Person String Int Color
    deriving Show

data Color = Red | Black | Green | Blue | Pink
    deriving Show

data PersonOrColor = CombinedPerson Person 
                   | CombinedColor Color
    deriving Show

orang = Person "Telkom" 3 Blue
warna = Red

personNameOrColorName :: PersonOrColor -> String
personNameOrColorName (CombinedPerson (Person name _ _)) = name 
personNameOrColorName (CombinedColor Red) = "Red"
personNameOrColorName (CombinedColor Black) = "Black"
personNameOrColorName (CombinedColor Green) = "Green"
personNameOrColorName (CombinedColor Blue) = "Blue"
personNameOrColorName (CombinedColor Pink) = "Pink"

whatIsNameField :: Person -> String
whatIsNameField p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

-- Abstract Data Type List

data IntList = Empty | Cons Int IntList
     deriving Show

intListku :: IntList
intListku = Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Empty))))

intListku2 :: IntList
intListku2 = Empty

intListProd :: IntList -> Int
intListProd (Empty) = 0
intListProd (Cons x Empty) = x * 1
intListProd (Cons x xs) = x * intListProd xs

data Tree = Leaf Int
          | Node Tree Int Tree
     deriving Show

dummyTree :: Tree
dummyTree = Node (Leaf 2) 1 (Node (Leaf 4) 3 (Leaf 5))

findInTree :: Int -> Tree -> Bool
findInTree i (Leaf j) = i == j
findInTree i (Node left j right) = findInTree i left || i == j || findInTree i right

-- recursive pattern polymorphism

-- mapping; perform some ops on every element on the list

addOneToAll :: IntList -> IntList
addOneToAll Empty = Empty
addOneToAll (Cons x xs) = Cons (x + 1) (addOneToAll xs)

absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons x xs) = Cons (x * x) (squareAll xs)

-- polymorphic map operations
mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList f Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

addOneToAll' :: IntList -> IntList
addOneToAll' = mapIntList (+ 1)

squareAll' :: IntList -> IntList
squareAll' = mapIntList (^ 2)

-- filtering input
keepOnlyPositive :: IntList -> IntList
keepOnlyPositive Empty = Empty
keepOnlyPositive (Cons x xs)
    | x > 0     = Cons x (keepOnlyPositive xs)
    | otherwise = keepOnlyPositive xs

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
    | even x    = Cons x (keepOnlyEven xs)
    | otherwise = keepOnlyEven xs

-- filter operation polymorphic
filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList p (Cons x xs)
    | p x    = Cons x (filterIntList p xs)
    | otherwise = filterIntList p xs

-- polymorphic data type list
data List t = E | C t (List t)
     deriving Show

-- polymorphic list map operations
mapList :: (t -> t) -> List t -> List t
mapList _ E = E
mapList f (C x xs) = C (f x) (mapList f xs)

-- polymorphic list filter operations
filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList p (C x xs)
    | p x    = C x (filterList p xs)
    | otherwise = filterList p xs

intListPoly = C 4 (C 4 (C 2 E))

