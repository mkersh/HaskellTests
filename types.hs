{-

The ability to create new algebraic data types is one of the really powerful features of Haskell.

Haskell supports type inference. Which means that you do not necessarily have to specify types yourself. You can let the system work them out for you.
This allows you to rapidly prototype, as you would in a language like python. Yet you still get the benefits of strong typing underneath.

There are a number of concepts associated with the type system in haskell that we will look at in this file:

* type inference
* explicit type definitions
* polymorphism
* data construct
* type construct
* typeclass
* newtype

-}

-- We allow Haskell to infer the types of all the following
-- To view the actual type chosen in ghci use :t (e.g :t label1)
-- See https://www.haskell.org/onlinereport/basic.html for details on standard types

-- Haskel infers the most expensive type for integers and chooses Integer
-- For label1 the smaller Int would possibly have been a better chose
label1 = 12

label2 = 3526252625262526252225

-- Inference chooses Double. We could have used a Float
label3 = 45.7

label4 = (22/7)

label5 = "Hello world"

label6 = [1,2,4,4,5,6]

label7 = ["One", "two"]

label8 = (1,2.0,(56/6),"This is it",'M')

-- ********************************
-- Explicitly defined types

label9:: Int
label9 = 67636

func1:: Int -> Int -> Int
func1 x y = x+y


func2:: (Int->Int)->[Int]->[Int]
func2 _ [] = []
func2 f (x:xs) = f x:func2 f xs

-- ********************************************************
-- Polymorphic types

-- The following is our first look at polymorphic types. 
-- These allow you to leave wholes in your data and function definitions that can be filled in by any suitable type
--
func3::(a->a)->[a]->[a]
func3 _ [] = []
func3 f (x:xs) = f x:func3 f xs

-- Although we did not need to for func3 above, quite often when referring to a polymorphic/generic type you need
-- to associate it to the typeclass(s) that it needs to support.
-- So for func4 below we are saying that a can be any type that supports the Num type class
func4::(Num a)=>a->a
func4 x = x + 5

-- ************************************************************
-- Creating your own types with data BB

-- In order to be able to use the new type: To make comparision with and to be able to print it needs
-- to support a number of standard type classes. We will see later how we can explicitly define these but for
-- now the easiest way to acheive this is by using the deriving mechanism.
data MyType = RED | GREEN | BLUE deriving (Eq, Show)

func5::MyType->String
func5 x = if x == RED then "It was Red" else "It was NOT red"

func6::MyType->MyType
func6 x = if x == RED then RED else x


