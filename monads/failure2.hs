{- Same as previous failure.hs with the exception that the composition is left to right,
as oppose to right to left in the previous example 
-}
module Failure where

divBy::Integer -> Integer -> Err Integer
divBy 0 y = Error
divBy x y = OK (div y x)

addTo::Integer -> Integer -> Err Integer
addTo x y = OK (x + y)


main = do
            print ((divBy 2 `composeErr` divBy 5) 2310)
            print ((divBy 0 `composeErr` divBy 7) 2310)
            print ((divBy 7 `composeErr` divBy 0) 2310)
            print ((divBy 5 `composeErr` divBy 11) 2310)
            print ((addTo 10 `composeErr` addTo 11 `composeErr` divBy 5) 100)
 
         
data Err a = OK a
            | Error deriving Show


-- (f . g) x :: (a->b) -> (b -> c) -> (a -> c)
-- NOTE: The composition signature is different when we execute left to right
composeErr :: (a-> Err b) -> (b->Err c) -> (a-> Err c)
composeErr f g x = case f x of
                    OK y -> g y
                    Error -> Error


-- Is the orginary function composition operator (.) left-right or right-left chaining?
-- We can figure this out by looking at its type (using :t) in ghci
-- It has the following type:
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- So that tells us it is right-left
-- i.e. (f1.f2.f3) x  - f3(x) will be executed first then f2, then f1
-- BUT the composition functions (i.e (.)) will always be executed left-right 

idErr :: a -> Err a
idErr x = OK x

