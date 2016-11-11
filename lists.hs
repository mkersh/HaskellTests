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







 