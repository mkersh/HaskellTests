-- Simplest function. Implicit Type definition and one expession
simpleNoExplicitType x = x * x

-- Simple function with explicit Type definition
simpleNoExplicitType2 :: Num a => a -> a
simpleNoExplicitType2 x = x * x

-- function with conditional code
func1 :: (Ord a, Num a) => a -> a
func1 x =
    if x < 100 then
        x*x
    else
        x

-- function with conditional code
func2 :: (Ord a, Num a) => a -> a
func2 x =
    if x < 100 then
        x*x
    else if x < 1000 then
        x + 100
    else
        x

-- More complex (and useful) function definitions use patterns and guards
-- First we will look at patterns
func4 :: (Ord a, Num a) => a -> a
func4 7 = 77777
func4 3 = 32323
func4 x =
    if x < 100 then
        x*x
    else
        x

-- Let's see another example of patterns incase the above wasn't clear
lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"   


factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  

-- The below is a very elegant definition of calculating the fibonacci numbers but it is horrendously
-- inefficient. My system grinds to a halt after ~ 34 terms
fib :: (Integral a) => a -> a  
fib 0 = 0
fib 1 = 1
fib x = fib(x-2) + fib(x-1)

fibSeq :: Integral b => b -> [b]
fibSeq n = map fib [0..n-1]


-- This next definition doesn't look as nice and is a lot harder to understand but it is a lot quicker
-- and I mean a lot quicker. The old fib above starts to struggle at fib 35 (takes about 1 min) fib 36 (takes 97 secs)
-- fib2 can go upto large numbers  fib2 100000 (takes 1.7 secs)
-- fib2 1000000 breaks it though with a Bus error: 10. Guess the problem is these numbers are getting very very large
-- fib2 500000 starts to get slow 24secs
-- numDigits (fib2 1000000) does work. So the Bus error I got previously must have been writing out the number which has 208988 digits 
-- in it (a fairly big number). It takes 98 secs in ghci
fib2' :: Integral t => t -> t -> t -> t -> t
fib2' n index prevMinus1 prevMinus2
    | n == 0 = 0
    | index == n = prevMinus1
    | otherwise = fib2' n (index+1) (prevMinus2 + prevMinus1) prevMinus1

fib2 :: Integral a => a -> a
fib2 x = fib2' x 1 1 0 

-- This next sequence is still not as efficient as it could be because it starts from scratch again for each term
-- really we just want to remember and have access to the previous elements already calculated
fibSeq2 n = map fib2 [0..n-1]

-- Just a useful little function to count the number of digits.
-- I wrote it to just see how huge the fibonnachi numbers actually get
-- fib2 100000 is a number with 20899 digits in it. That's a lot bigger than the estimates atoms in the universe (10^79-10^82)
-- fib2 800000 : calculates in 63 secs and result has 167190 digits in it
--
-- To use on ghci: numDigits $ fib2 100000
-- NOTE: our use of $ to control the precedence (something I don't like about haskell)
-- we could have done numDigits (fib2 100000) making it more like lisp 
numDigits' 0 acc = acc
numDigits' x acc = numDigits' (x `div` 10) (acc+1)

numDigits x = numDigits' x 0

-- The accumulator pattern that I have used above is very important in functional/declarative programming.
-- I do remeber this from my prolog days (almost 30 years ago!)


-- Just found another method on google for calculating fib. The author claims it is very quick and can calculate fib 10^1000 in less than 1 secs
-- Actually on closer inspection he doesn't claim it will calculate fib 10^1000. He claims it calculated numbers with 10^1000 digits in less than 1 sec
-- http://stackoverflow.com/questions/18172257/efficient-calculation-of-fibonacci-series 
-- Let's see if it works
-- It works but it is not as quick as fib2 for larger numbers
-- fib3 !! 1000000 -- Is not coming back. This will be building up a huge list though with very big numbers on it. Guess that's the problem.
fib3 = 0 : (f 1 1) where f a b = a : f b (a+b)

-- fib3 is an infinite sequence. To return a specific element use !!
-- fib3 !! 1000000 -- will return the millionth item in the sequence

fib3Item x = fib3 !! x

-- fibSeq3 is quicker than fibSeq2 until we start to get bigger numbers 
fibSeq3 n = map fib3Item [0..n-1]

-- fibSeq4 is same as fibSeq3 except it is using a lambda function to calculate the element at a particular position
fibSeq4 n = map (\x -> fib3 !! x) [0..n-1]

firstN' :: (Integral t) => [t] -> Int -> [t] -> [t]
firstN' [] n resList = resList
firstN' (x:xs) n resList
    | n == length(resList) = resList -- If t2 typed as Integral then you need fromIntegral(length(resList))
    | otherwise = firstN' xs n (x:resList)

firstN xs n =
    reverse (firstN' xs n [])

-- Thought the below was going to be quick but its not. Its slower than fibSeq2, fibSeq3, fibSeq4 (for n==1000)
fibSeq5 n = firstN fib3 n

-- ****************************************
-- Function guards

-- I have already used these above in my fibonnachi experiments but here are some simplified examples to make sure the concept is clear

bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  

bmiTell2 :: (RealFloat a) => a -> String  

-- If there is no otherwise guard and none of the other guards match it will move onto the next pattern
bmiTell2 bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  

bmiTell2 bmi  = "You're a whale, congratulations!"  

-- Very artificial example to demo patterns, guards
listeg:: Integral t => [t] -> [Char]
listeg [] = "Empty List"
listeg [n] = "One Item"
listeg [x,y] = "Two Items"
listeg [x,y,z] = "Three Items"
listeg xs
    | length(xs) == 4 = "Four Items"
    | length(xs) == 5 = "Five Items"
listeg (x:xs)
    | (length(xs)+1) == 6 = "Six Items"
    | otherwise = "More than Six items!!!"


-- My first example of a where binding
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname    


-- You can also create functions in the where
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  

myFunc:: Integral t => t -> t
myFunc x =  myFuncHelper1 x -- NOTE myFuncHelper2 is not visible in myFunc
    where myFuncHelper1 y = myFuncHelper2(y*y) where myFuncHelper2 z = z + 2

myFunc2:: Integral t => t -> t
myFunc2 x =  myFuncHelper2 (myFuncHelper1 x) where 
    myFuncHelper1 y = (y*y)
    myFuncHelper2 z = z + 2   -- The syntax here appears very messy though. The two wheres have to line up exactly??

myFunc3:: Integral t => t -> t
myFunc3 x =  myFuncHelper1 x where 
    myFuncHelper1 y = myFuncHelper2(y*y) where 
        myFuncHelper2 z = z + 2


-- where clauses are in scope across all guards in a function
msg n
    | n == 1 = "One!!" ++ helloWorld
    | n == 2 = "Two!!" ++ helloWorld
    | n == 3 = "Three!!" ++ helloWorld
    | otherwise = "Did not match Hello anyway!"
    where
        helloWorld = "Hello World"  

-- let clauses are similar to where but more local
-- they can not span across guards
myFunc4:: Integral t => t -> t
myFunc4 x =  
    let myFuncHelper1 y = myFuncHelper2(y*y) 
        myFuncHelper2 z = z + 2
    in myFuncHelper1 x 


-- case expressions
-- NOTE: We could do the same with function patterns and/or guards but case expressions can be used anywhere
headTest :: [a] -> a  
headTest xs = 
    case xs of 
        [] -> error "No head for empty lists!"  
        (x:_) -> x  


-- More recursive examples from the tutorial
-- This is similar to my firstN above but not as complicated
-- Probably not as good though because in FirstN I was using the accumulatorPattern with tailRecursion
-- WRONG: This is a lot quicker than my FirstN
-- It will actually work with length $ testTake 1000000
take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  

testFirstN n = length $ firstN [1..] n

testTake n = length $ take' n [1..]

-- What is the problem with firstN. Let's try re-writing
-- It is the length call that is slowing it down.
-- The below version is much better but still not quite as fast as take'
firstN2' :: (Integral t) => [t] -> Int -> Int -> [t] -> [t]
firstN2' [] n tmpN resList = resList
firstN2' (x:xs) n tmpN resList
    | n == tmpN = resList 
    | otherwise = firstN2' xs n (tmpN+1) (x:resList)

firstN2 xs n =
    reverse (firstN2' xs n 0 [])


testFirstN2 n = length $ firstN2 [1..] n

-- *********************************************************
-- Pipeline

-- Let's try and create a simple pipline

sumLists::(Integral t)=>[t]->[t]->[t]
sumLists l1 l2 =
    let
        step1 = map (+3) l1
        step2 xs = map (*2) xs
        in
            step2 step1


sumLists2::(Integral t)=>[t]->[t]->[t]
sumLists2 l1 l2 =
    let
        step1 = map (+3) 
        step2 = map (*2) 
        in
            step2.step1 $ l1

sumLists3::(Integral t)=>[t]->[t]->t
sumLists3 l1 l2 =
    let
        step1 = map (\(x,y)->x+y) 
        step2 = head
        step3 x = (x+7) 
        step3b = (+7) -- step3 and step3b are identical 
        in
            step3.step2.step1 $ zip l1 l2

sumLists4::(Integral t)=>[t]->[t]->t
sumLists4 l1 l2 =
    step3.step2.step1 $ zip l1 l2
    where
        step1 = map (\(x,y)->x+y) 
        step2 = head
        step3 x = (x+8) 

sumLists5::(Integral t)=>[t]->[t]->[t]
sumLists5 l1 l2 =
    -- (step2.step1) (zip l1 l2) -- This works
    --step2.step1 zip l1 l2 -- Doesn't work
    --(step2.step1) zip l1 l2 -- Doesn't work
    step2.step1 $ zip l1 l2 -- works
    where
        step1 = map (\(x,y)->x+y) 
        step2 = map (*2)

sumLists6::(Integral t)=>[t]->[t]->[t]
sumLists6 l1 l2 =
    (step2.step1)  dataFeed
    where
        someConst = 3   -- wasn't sure if you would be able to do this because it rules out where clauses being executed in parallel
        dataFeed = zip l1 l2
        step1 = map (\(x,y)->x+y) 
        step2 = map (*someConst)


sumLists7::(Integral t)=>[t]->[t]->[t]
sumLists7 l1 l2 =
    let
        someConst = 3   -- Just testing allowed here as well 
        dataFeed = zip l1 l2
        step1 = map (\(x,y)->x+y) 
        step2 = map (*someConst)
        in 
            (step2.step1)  dataFeed


fn1:: [Int] -> Int
fn1 = head.(map (+2))

fn2:: Int -> Int
fn2 = (*3)














