module Destruction where
-- These imports are not needed for the main test here, just at the end where we show 
-- how you can convert our composition function into an official haskell monad
import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

waitForInput :: String -> IO String
-- This is the first time I have seen the { ; } syntax. We could have done this on seperate lines as well
waitForInput s = do { putStrLn s; getLine }
-- We could have written waitForInput as:
{-
waitForInput s = do 
    putStrLn s
    getLine
-}
-- A reference to this: https://gist.github.com/CMCDragonkai/1a241955b041283a9009

outputReverse :: String -> IO ()
outputReverse s = putStrLn (reverse s)

main = do
    s <- waitForInput "Enter some text:"
    outputReverse s

data IOAction a = Output String (IOAction a) 
                | Wait (String-> IOAction a)
                | Return a -- deriving (Eq, Show) -- You cannot simply derive show for this type

runIO :: IOAction a -> IO a
runIO (Output s n) = do {putStrLn s; runIO n}
runIO (Wait f) = do {s <- getLine; runIO (f s)}
runIO (Return x) = do {return x}

-- Simple test of runIO
testRunIO = runIO (Output "Foo" (Return ()))
testRunIO2 = runIO (Wait (\s -> (Return s)))

composeIO :: (b -> IOAction c) -> (a -> IOAction b) -> (a -> IOAction c)
composeIO f g x = case (g x) of
                    Output s n -> Output s (composeIO f (\_ -> n) ())
                    Wait h -> Wait ( \s -> composeIO f h s)
                    Return r -> f r

idIO :: a -> IOAction a
idIO x = (Return x)

bindIO :: (IOAction a) -> (a -> IOAction b) -> (IOAction b)
bindIO e f = (composeIO f id) e

-- Now let's make this an official haskell monad
instance Monad IOAction where
    return x = idIO x
    (>>=) = bindIO

-- BUT You now need to support the Functor and Applicative classtype as well. This came into play since GHC 7.10
-- There are a lot of tutorials that no longer compile because of this. Including the one I was using.
-- See http://stackoverflow.com/questions/31652475/defining-a-new-monad-in-haskell-raises-no-instance-for-applicative

-- The below are generic definitions of these that work for any monad (so I am told)
instance Functor IOAction where
  fmap = liftM

-- Again this is just a generic definition that works for any monad. See link above for details
instance Applicative IOAction where
  pure  = return
  (<*>) = ap

-- Didn't think I was going to be able to define Show for IOAction. I was getting a load of errors
-- Turned out all I had to do in the end was (IOAction a)
instance Show (IOAction a) where
    show (Output s n) = "IOAction Output - " ++ s ++ "\n" ++ show n
    show (Wait f) = let x = (f "hello") 
                        in "IOAction Wait \n"
    show (Return _) = "IOAction Return - \n" 

-- I am really not sure what the purpose of this destructive monad is??
-- It is a little clearer after I made it into a real haskell monad and wrote the testMonad below BUT I still don't know what the purpose is for this
testMonad = do
    a1 <- (Output "One at front gets printed" (Return ()))
    x <- (Output "Foo" (Return "FooStr"))
    y <- (Output "Hello World") (return x)
    (Output y) (return x)
    -- This wait is not waiting
    Wait (\s -> (Return s))
    (return y)

