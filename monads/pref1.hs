{-
This is an example has hardcoded preferences embedded within it.
In pref2.hs we show how to use a Monad to pass this information around.
-}

module Dependence where

-- We are using a global to contain our preference information and pass to
-- the functions below. This is not a good idea though and makes the
-- functions hard to read. 
i = 5 :: Integer

main = putStrLn(f "foo")
    where f = left . right

left :: String -> String
left s = (repeatString i "< ") ++ s

right :: String -> String
right s = s ++ (repeatString i " >") 

repeatString :: Integer -> String -> String
repeatString i s = if (i <=0)
                    then ""
                    else s ++ repeatString (i - 1) s

