module Failure where

divBy::Integer -> Integer -> Err Integer
divBy 0 y = Error
divBy x y = OK (div y x)

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
