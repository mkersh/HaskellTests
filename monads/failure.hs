module Failure where

-- These imports are not needed for the main test here, just at the end where we show 
-- how you can convert our composition function into an official haskell monad
import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

divBy::Integer -> Integer -> Err Integer
divBy 0 y = Error
divBy x y = OK (div y x)

addTo::Integer -> Integer -> Err Integer
addTo x y = OK (x + y)

temp = divBy 5 `composeErr` divBy 0
temp1 = temp 2310

temp2 = divBy 1 `composeErr` temp

main = do
            print ((divBy 2 `composeErr` divBy 5) 2310)
            print ((divBy 0 `composeErr` divBy 7) 2310)
            print ((divBy 7 `composeErr` divBy 0) 2310)
            print ((divBy 5 `composeErr` divBy 11) 2310)
            print ((divBy 1 `composeErr` divBy 5 `composeErr` divBy 0) 2310)
            print ((divBy 1 `composeErr` temp) 2310)
            print ((divBy 2 `composeErr` divBy 1 `composeErr` divBy 5 `composeErr` divBy 0) 2310)
            print ((divBy 2 `composeErr` temp2) 2310)
            -- The result of this next one is different from when we compose left to right (see failure2.hs)
            print ((addTo 10 `composeErr` addTo 11 `composeErr` divBy 5) 100)
            
            -- The following will not compile
            --print ((divBy 1 `composeErr` temp1))

-- The difficult for me is visualising how the composition actually works and how Errors are propagated up the composition chain
-- Consider the composition chain (f4.f3.f2.f1) and we are feeding x into it
-- 1st composition is f2.f1 with x passed in. So g = f1 and f = f2
--      This will either return Error or f(g(x))
-- 2nd composition is f3.(1st composition). so g = (1st composition) ad f = f3
--      The result of the (1st composition) immediately becomes the result of g x
--          If you can accept this then it is obvious how the composition chain works
--      BUT I'm struggling to understand how the compiler handles this because the composeErr functions needs two functions and an x passed in
--          Think I might have just had my eureka moment
--          Heres how to understand it:
--              When you chain functions together the parameter gets passed to the left most function.
--                  For normal composition this parameter will then get passed down to the right function. 
--                  This results in the leaf functions on right executing first and passing their results back
--       
--          
--          
data Err a = OK a
            | Error deriving Show


-- (f . g) x :: (b->c) -> (a -> b) -> (a -> c)
composeErr :: (b-> Err c) -> (a->Err b) -> (a-> Err c)
composeErr f g x = case g x of
                    OK y -> f y
                    Error -> Error

-- We don't need the identity function below BUT for monads in general you need to define this
idErr :: a -> Err a
idErr x = OK x

bindErr :: (Err a) -> (a -> Err b) -> (Err b)
bindErr e f = (composeErr f id) e

-- Trying to make this an official haskell monad
-- Originally all you had to do was 
instance Monad Err where
    return x = idErr x
    (>>=) = bindErr

-- BUT You now need to support the Functor and Applicative classtype as well. This came into play since GHC 7.10
-- There are a lot of tutorials that no longer compile because of this. Including the one I was using.
-- See http://stackoverflow.com/questions/31652475/defining-a-new-monad-in-haskell-raises-no-instance-for-applicative

-- The below are generic definitions of these that work for any monad (so I am told)
instance Functor Err where
  fmap = liftM

-- Again this is just a generic definition that works for any monad. See link above for details
instance Applicative Err where
  pure  = return
  (<*>) = ap

-- Not really sure yet how to use the official monad syntax. 
-- Got the basics working with the following
testMonad = do
    x <- (divBy 2) 400
    y <- (divBy 3) x
    (return y)

-- This next one shows the error getting propagated down the chain
-- Remember that the do syntax is just syntactic sugar, each line results in the bind operator >>= being called, which links to our composeErr 
testMonad2 = do
    x <- (divBy 0) 400
    y <- (divBy 3) x
    (return y)

