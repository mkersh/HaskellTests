{- 
This is a multiline comment in haskell. I don't use them much but just wanted to test one out
-}
-- The following imports are needed for my pascal traingle test ptri2
import qualified Data.Sequence as DS
-- To make it easier to read import DS operator into namespace directly. So we do not have to qualify them with DS when using
import Data.Sequence ((|>), (<|))   

-- In this experiment we are going to look at higher order List functions that make it easier to navigate and manipulate lists
-- Using http://learnyouahaskell.com/higher-order-functions as a reference

-- NOTE: You should introduce these HO functions for all Data structures that you introduce. 
-- This allows you to hide the complexity of the underlying data structure. This becomes important when you start to use more sophisticated data structures


doubleMe x = x + x

-- ***************************************
-- map function
--
-- takes a funtion and a list and applies the function to each item in the list to create a new list

mapTest1 = map doubleMe [1,2,3,4,5]

-- Rather than using a named function this next example uses an anonymous lambda function to define the function to apply
mapTest2 = map (\x->x*2) [1,2,3,4,5]

-- This next one is difficult to understand when you're new to FP
-- It is usinf function currying to define the function
-- (*2) is the standard * operator but is missing the left side parameter. This return a function that needs this final parameter.
-- and this is supplied when the map iterates though the list
mapTest3 = map (*2) [1,2,3,4,5]

-- To understand currying better consider the following function
myFunc x y z = x + y + z

-- myFunc returns a function with x=1
myFunc1 = myFunc 1

-- myFunc returns a function with x=1 and y = 2
myFunc2 = myFunc 1 2

-- The next function uses currying to create a specific list function that uses map
-- because we didn't pass a list to map it still requires this to be passed
listDouble = map doubleMe

-- We could have writeen listDouble more explicitly as
listDouble' lst = map doubleMe lst


-- ***************************************
-- filters function
--
-- takes a funtion and a origList and returns a newList that contains elements from origList that return true when function called

-- following caused me problems because:
--     I missed the `` around mod to make it an operator
--     The type a has to be an integral for mod to work
filterCond :: (Ord a, Integral a) => a -> Bool
filterCond x = (x `mod` 2) == 0

-- The following will return all even numbers from the list
filterTest1 = filter filterCond2 [1,2,3,4,5,6,7,8,9,10]

filterCond2 x = (mod x 2) == 0
filterTest2 = filter filterCond2 [1,2,3,4,5,6,7,8,9,10]

-- ***************************************
-- fold functions - foldr and foldl
--
-- The fold functions implement the accumulator pattern.
-- They go through a list applying a function to each item in the list and an accumulator value.
-- The new accumulator is updated with the value returned by the function

foldtest = foldr (+) 0 [1,2,3,4,5,6,7,8,9,10]

-- For addition foldr and foldl will hav the same results
foldtest2 = foldl (+) 0 [1,2,3,4,5,6,7,8,9,10]

-- Lets work out the different between foldr and foldr

-- for a foldr the 2nd parameter is the accumulator, 1st is the list item
reduceFunc x lst = x*2:lst
-- foldr starts from rightmost element and moves to the left
foldtest3 = foldr reduceFunc [] [1,2,3,4,5,6,7,8]

-- for a foldr the 1st parameter is the accumulator, 2nd is the list item
reduceFunc2 lst x = x*2:lst
-- foldl starts from the left most element and moves right
foldtest4 = foldl reduceFunc2 [] [1,2,3,4,5,6,7,8]

-- foldl1 and foldr1 are similar to foldr and foldl but do not require you to pass the initial accumulator
-- The initial accumulator is taken as the first item in the list
foldtest5 = foldl1 (\acc x -> (x+acc)) [1,2,3,4,5,6,7,8]
foldtest6 = foldl1 (+) [1,2,3,4,5,6,7,8]


head' :: [a] -> a  
head' = foldr1 (\x _ -> x) 

-- ************************************
-- scanl and scanr
-- similar to foldl and foldr but report all intermediate results as a list

scantest1 = scanl (+) 0 [1,2,3,4,5,6,7,8,9,10]

scantest2 = scanl1 (+) [1,2,3,4,5,6,7,8,9,10]


-- ***************************************
-- Zip
-- Zip allows for to combine two lists together

-- NOTE: It takes corresponding items from both lists and forms a tuple
-- The number of items is the same as in the smallest lists
ziptest1 = zip [1..] "Hello World"

-- combine two infitite lists
-- To print this out in ghci recommend you use take 100 ziptest2 (else it will never complete)
ziptest2 = zip [1..] [1..]

-- Rather than combine into a tuple you can use zipWith and choose the function you want to combine with
ziptest3 = zipWith (+) [1,2,3,4,5] [1,2,3,4]


-- Alternative version of ziptest3 which is probably easier to read
-- This one uses an anonymous lambda functions to make it more explicit that the 2nd param to zipWith is a function taking two parameters
ziptest4 = zipWith (\x y-> x+y) [1,2,3,4,5] [1,2,3,4]

-- We could of written something similar to zipWith using a combination of zip and map
zipWith1' f l1 l2 = map f (zip l1 l2)

-- It is not exactly the same because  our function takes a tuple with the two values passed it whereas zipWith function is passed two seperate params
ziptest5 = zipWith1' (\(x,y)->x+y) [1,2,3,4,5] [1,2,3,4]

-- Let's correct the difference between our zipwith and the official one by using an intermediate lambda function
zipWith2' f l1 l2 = map (\(x,y)->f x y) (zip l1 l2)

ziptest6 = zipWith2' (+) [1,2,3,4,5] [1,2,3,4]

-- We could have implemented zipWith at a lower level of course
-- We will do this using the accumulator pattern
zipWith3Aux:: (a->b->c)->[a]->[b]->[c]->[c]
zipWith3Aux _ [] _ acc = acc
zipWith3Aux _ _ [] acc = acc
zipWith3Aux f (ax:axs) (bx:bxs) acc = zipWith3Aux f axs bxs (f ax bx:acc)

-- Make it easier to call zipWith3' by hiding the need to pass an initial [] accumulator into the function
-- Was originally calling my function zipWith3 but turns out this clashes with a standard library function in Prelude
-- So presumably this standard version combined 3 lists together
zipWith3':: (a->b->c)->[a]->[b]->[c]
-- results from my zipWith3Aux end up in reverse order. This is because I am appending new results to the front of the accumulator
-- rather than the end because this is faster.
-- It does mean I have to do a reverse at the end, which I am assuming is quicker than constantly having to recreate the string as we are building it
zipWith3' f a b = reverse (zipWith3Aux f a b [])

ziptest7 = zipWith3' (+) [1,2,3,4,5] [1,2,3,4]


-- Pascals Triangle (from http://stackoverflow.com/questions/5188286/idiomatic-efficient-haskell-append)
-- The below is probably the most complex haskell recursion that I have so far come across.
-- At the moment I can't understand it. The thing that is confusing me is the fact the that rows gets instantiated to the newRow that we are currently
-- building up. This is black magic!! I have never seen anything like this before
ptri = [1] : mkptri ptri
mkptri (row:rows) = newRow : mkptri rows
    where newRow = zipWith (+) row (0:row) ++ [1]


-- Let me play with the concept a little
testList1 = 1 : intGen testList1

-- Yes it bloody works. The xs gets instantiated to the value you are currently calculating.
-- i.e. The x*2 and the future values from the recursive call
intGen (x:xs) = x*2: intGen xs


-- Let me play with the concept a little
--testList2 = (1,99) : intGen2 testList2
-- There is a limit to what you can do with the future xs (which makes sense)
-- You can't for example start to take future values from it
-- intGen2 ((x,_):xs) = (x*2, xs!!0) : intGen2 xs

-- Back to Pascals triangle
-- The guy on stackoverflow was obsessed with the fact that he was append at the end of the string and that this is not efficient.
-- He wanted a better way. He liked the following


-- If you import zipWith into the namespace then you need to hide the standard Prelude one
--import Prelude hiding (zipWith)

-- Again the initial first thoughts are WTF!!!!

-- OK so I can understand this. Need to use singleton to make it a Data.Sequence
ptri2 = DS.singleton 1 : mkptri2 ptri2

-- Well pretty obvious really once you look closely and realise that we are using overloaded |> and <| operators that are
-- appending to beginning or end of a sequence
mkptri2 (seq:seqs) = newRow : mkptri2 seqs
    where newRow = DS.zipWith (+) seq (0 <| seq) |> 1  -- NOTE: You can access the Data.Sequence operators via D.<| but this does not look nice


-- How much quicker is Data.Sequence than an Ordinary List. Let's do some tests

testds:: Int -> DS.Seq Integer -> DS.Seq Integer
testds 0 acc = acc
testds n acc = testds (n-1) (acc |> 1)

testitds = DS.length $ testds 100000000 (DS.singleton 1)

testds2:: Int -> DS.Seq Integer -> DS.Seq Integer
testds2 0 acc = acc
testds2 n acc = testds2 (n-1) (1 <| acc) 

testitds2 = DS.length $ testds2 100000000 (DS.singleton 1) 


testds3:: Int -> [Integer] -> [Integer]
testds3 0 acc = acc
testds3 n acc = testds3 (n-1) (acc ++ [1])

testitds3 = length $ testds3 30000 [1]

testds3b:: Int -> [Integer] -> [Integer]
testds3b 0 acc = acc
testds3b n acc = testds3b (n-1) (1:acc)

testitds3b = length $ reverse (testds3b 10000000 [1])

testds4:: Int -> [Integer] -> [Integer]
testds4 0 acc = acc
testds4 n acc = testds4 (n-1) (1:acc) 

testitds4 = length $ testds4 100000000 [1]


-- *********************************************
-- Another form of paramater decomposition

-- Saw the following this morning for the first time

tuple1 = ("SomeName", 99)
-- The @ operator allows you to match entire parameter and also decompose
tupleFunc tup@(nm,val) = val:[]
tupleTest = tupleFunc tuple1


-- ***********************************************
-- Another way to calculate fibonnachi
fib = 0:1: genFib fib
genFib (x:y:xs) = x+y: genFib (y:xs)
-- Still prefer the one I found yesterday
fib2 = 0 : (f 1 1) where f a b = a : f b (a+b)

-- Interestingly with the above is that if you pass xs into the recusive call to genFib it stalls after 0,1,1 and never returns
list65 = 0:1:2:genList65 list65
-- It does make sense if you think about it. Just surprised that it is stalling
genList65 (x:y:z:xs) = x+y+z : genList65 (y:z:xs)


