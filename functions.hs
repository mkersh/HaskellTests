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







