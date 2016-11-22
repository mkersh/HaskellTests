{-
This is a refactoring of pref1.hs to use a monad like structure 
to pass dependency information between functions in a composite chain.
-}

module Dependence where


main = putStrLn(f "foo" 5)
    where f = left `composePref` right `composePref` right

left :: String -> Pref String
-- The change here is that left is now a higher order function
-- It needs to be passed an extra parameter which is the config information
left s = \i -> (repeatString i "< ") ++ s
-- Although the above is the natural way to write this. We could have written
-- This would do the same thing BUT it isn't as obvious that it matches the signature left :: String -> Pref String
-- left s i = (repeatString i "< ") ++ s

-- So as for left we are injected configuration into the function.
-- How good is that!!!
-- and that my friend is one of the classic uses of monads.
-- rememeber the 4 horsemen of the catapocalypse (failure, dependency, non-determinism, destruction)
--      These are the four things that pure functions struggle with but that monads can help solve   
right :: String -> Pref String
right s = \i -> s ++ (repeatString i " >") 

repeatString :: Integer -> String -> String
repeatString i s = if (i <=0)
                    then ""
                    else s ++ repeatString (i - 1) s


-- In this simple example out config is just a single Integer
-- In a real world example it would be more complex than this.
type Config = Integer

-- Pref is our container for adding preferences to other functions
type Pref a = (Config -> a)

-- Can we see the pattern emerging for what a monad is
composePref :: (b -> Pref c) -> (a -> Pref b) -> (a -> Pref c)

composePref f g x = \c -> let y = (g x) c
                            in    (f y) c  

-- We have used a lambda within the body to accept the preference param c
-- We could have just had c as an extra parameter on composePref:
--composePref f g x c = let y = (g x) c
--                            in    (f y) c  

-- Again in our example we do not need the identify function but for monads this needs to be defined
idPref :: a -> Pref a
idPref x = \_ -> x
