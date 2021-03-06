-- My experiments in Haskell whilst following the Tutorial: http://learnyouahaskell.com/starting-out

-- ************************************************************************************
-- ********* First experiments with functions

-- |doubleMe doubles an item passed in
doubleMe x = x + x  

doubleSmallNumber x = 
    if x > 100  
    then x+1 
    else x*2   

-- Funny characters in names
conanO'Brien = "It's a-me, Conan O'Brien!"  

-- Functions without parameters are names. They are constant though

mark1 = "This is it"
-- In Haskell or any pure functional programming language names onces declared are immutable
-- So we would not beable to overload mark1. The below if you comment out will cause an error
--mark1 = "This is it redefined" 

func = let lostNumbers = [4,8,15,16,23,42]
    in lostNumbers

-- ************************************************************************************
-- ********* LISTS


-- Appending strings (strings are just lists of characters)

str1 = "Hello " ++ "World"

str2 = mappend "Hello " "World2"

str3 =  "Hello " `mappend` "World2"

-- Appending is slow though if the left list is large

-- Concat an item to the beginning of a list

list1 = 1:[2,3,4]

-- Getting an item out of a list using !!
item1 = [6,7,8] !! 1 -- starts at 0

indList = let b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
    in b

-- ************************************************************************************
-- ********* LIST Comprehension

list2 = [x*x | x<-[1..10]]
list3 = [x*x | x<-[1..10], x `mod` 2 == 0]
list4 = [(x,x*x) | x<-[1..], x `mod` 2 == 0]  -- Infinite list that will go on forever

list5 = [(x,y,x*y) | x<-[1..10], y<-[1..10]]

-- Next set of functions to produce a unique permutation is the first thing in heskell
-- I have written that made me smile

-- The next set2' function is a brief preview of function patterns which we will cover in more details later
-- NOTE: This nottation is very similar to how you would do it in prolog
set2' [] lst2 = lst2
set2' (hd:tl) lst2 = set2' tl (set' hd lst2)

-- set' add an element to a set
set' x lst =
    if x `elem` lst then lst else x:lst

-- perm1 below are all the unique permutations of 0..7.
-- This is the classic way to calculate the 8 queens problem
-- all that is missing is to check elements are not on same 
perm1 = [(x1,x2,x3,x4,x5,x6,x7,x8)| 
    x1<-[0..7], 
    x2<-[0..7], 
    x3<-[0..7], 
    x4<-[0..7], 
    x5<-[0..7], 
    x6<-[0..7], 
    x7<-[0..7], 
    x8<-[0..7],
    length(set2' [x1,x2,x3,x4,x5,x6,x7,x8] []) == 8
    ]

-- perm2 is a list of solutions to the 8 queens problem
perm2 = [(x1,x2,x3,x4,x5,x6,x7,x8)|
    (x1,x2,x3,x4,x5,x6,x7,x8) <-perm1,
    -- The next two predicates check that nothing is on the same diagonal
    (length (set2' [x1,x2+1,x3+2,x4+3,x5+4,x6+5,x7+6,x8+7] [])) == 8,
    (length (set2' [x1,x2-1,x3-2,x4-3,x5-4,x6-5,x7-6,x8-7] [])) == 8
    ]

-- Let's generate as a single list comprehension. Should be quicker
perm3 = [(x1,x2,x3,x4,x5,x6,x7,x8)| 
    x1<-[0..7], 
    x2<-[0..7], 
    x3<-[0..7], 
    x4<-[0..7], 
    x5<-[0..7], 
    x6<-[0..7], 
    x7<-[0..7], 
    x8<-[0..7],
    length(set2' [x1,x2,x3,x4,x5,x6,x7,x8] []) == 8,
    (length (set2' [x1,x2+1,x3+2,x4+3,x5+4,x6+5,x7+6,x8+7] [])) == 8,
    (length (set2' [x1,x2-1,x3-2,x4-3,x5-4,x6-5,x7-6,x8-7] [])) == 8
    ]

-- Although the above look very nice, they are incredibly slow when compared 
-- even to a language like python

-- Some more list comprehension examples

rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]  

-- The following types of inferred list comprehension are pretty mind blowing. I've not seen anything
-- like this is other langauges

list6 = [100,99..1] -- OK so maybe that one is not too clever but ...

list7 = [2,4..100]

-- ************************************************************************************
-- ********* TUPLES

pair1 = (1,2)

ans1 = fst pair1 -- fst only works on a pair tuple (not one with more or less elements)
ans2 = snd pair1

-- zip example
zip1 = zip [1..] "Hello World. This is me"  

-- ************************************************************************************
-- ********* FUNCTIONS

-- declaring the type explicitly (considered good practice)
removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   

addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z

-- Generic version
addThree2 :: (Num a) => a -> a -> a -> a  
addThree2 x y z = x + y + z

factorial :: Integer -> Integer  
factorial n = product [1..n]  


main = print (perm3)

