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
-- Defaults on Record fields
import Data.Default

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


-- ***********************************************************
-- Let's start to look at more interesting data types

-- This is a Tree DS. See tree.hs for a set of API and tests on this
-- NOTE: This is an example of a parameterised Type, or a Type constructor
-- You cannot use Tree as a type anywhere, you have to make a concrete type out of it
-- by specifying what the type of the parameter is e.g (Tree String) or (Tree Int)
data Tree a = NIL | Tree a (Tree a) (Tree a) deriving (Show)


-- MyRec1 defines a record structure type
-- NOTE: The MyRec1 to the left of the = is the Type. MyRec1 to the right is the Value constructor.
-- In this case we are using the same name but they do not have to be. See MyType above.
--
-- Types created using the haskell data mechanism are referred to as algebraic data types.
-- The algebraic refers to the fact that you can specify the types using AND and OR algebra
-- data MyType = Val1 INT INT | Val2 String OtherType
--
data MyRec1 = MyRec1 String Int Int deriving (Show)

data1 = MyRec1 "Hello World" 1 2

-- The above MyRec1 works but it is no very readable. The fields of our Value MyRec1 are just referred to as String, Int and Int
-- To make it more readable we can use the haskell type mechanism to create named aliased for types
type CustomerName=String
type ID=Int
type Age=Int
data MyRec2 = MyRec2 CustomerName ID Age deriving (Show)

data2 = MyRec2 "Hello World" 1 2

-- type aliases make MyRec2 slightly more readable but it is still not great. When we call the type we still need to pass parameters to the value constructor
-- in the correct order. i.e. CustomerName first, ID second etc

-- Another possibility is to use Record syntax

data MyRec3 = MyRec3 {companyName :: CustomerName, custId :: ID, age :: Age} deriving (Show)

data3 = MyRec3 {companyName="Oracle systems", custId=5678, age=50}

-- You can then pattern match to pull things out of the structure
-- Below gets the companyName field into theName
MyRec3 {companyName=theName, custId=_, age=_} = data3

-- You can just select the fields that you want as well (which is nice)
MyRec3 {companyName=theName2} = data3

-- Defaults on Record fields
-- NOTE: The fields in the record need to be uniquely named. So for this artificial test I had to add 2 to the end of each else they clash with
-- names on MyRec3
data MyRec4 = MyRec4 {companyName2 :: CustomerName, custId2 :: ID, age2 :: Age} deriving (Show)

-- You need import Data.Default
-- NOTE: This is not installed by default on a standard haskell ghci environment.
-- You need to install using the haskell package manager cabal
-- cabal install data-default
-- You also need to stop/start the ghci terminal
instance Default MyRec4 where
  def = MyRec4 "Default CustName" 9999 21
          -- or whatever you want as the defaults

data4 :: MyRec4
data4 = def { companyName2 = "Acorn engineering" }


-- Discussion on defaults http://stackoverflow.com/questions/31965666/how-do-i-set-default-values-in-a-record

-- rather than use Data.Default you can just have your own default constructor
-- I LIKE THIS. THIS would be my preference
defRec = MyRec4 "Default CustName" 9999 21
data5 = defRec { companyName2 = "Acorn engineering22" }

-- The following would not work because you haven't given values for all the fields
-- data6 = MyRec4 { companyName2 = "Acorn engineering33" } 