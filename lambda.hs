{-
I have been following the lambda calculus course from Arizona State University https://www.youtube.com/watch?v=_kYGDJSm0gE.

These are experiments to try out some of the ideas in haskell.

Reminder: In pure lamda calculus lambda expression are made up from the following rules

E -> ID
E -> \x. E     -- This is a function definition aka an abstraction
E -> E E       -- This is a function application aka a function call
E -> (E)

-}

import Unsafe.Coerce
import Data.Function (fix)

-- ********************************************
-- Binary Logic

true = (\x->(\y->x))
false = (\x->(\y->y))

-- This will output results for us in human readable form
printBool b = 
    b "TRUE" "FALSE"

-- AND function
-- To understand this you need to do Beta reduction on the arguments
-- If we pass T T where T = true:
-- The body of the boolAND ends up as T T F
-- a T always returns its first argument and disregards its 2nd. So above results in T
-- If we pass F T then:
--  The body of the boolAND ends up as F T F
--  an F always returns its second argument and disregards its 1st. So the above results in F 
boolAND = (\a->(\b->a b false))

testAnd = do
    print "AND true true = "
    print $ printBool $ boolAND true true
    print "AND true false = "
    print $ printBool $ boolAND true false
    print "AND false true = "
    print $ printBool $ boolAND false true
    print "AND false false = "
    print $ printBool $ boolAND false false

-- NOT function
-- If a is T it will return 1st argument i.e. false. If a is F it will return 2nd argument i.e. T
boolNOT = (\a->a false true)

testNOT = do
    print "NOT true = "
    print $ printBool $ boolNOT true
    print "NOT false = "
    print $ printBool $ boolNOT false


-- OR function
boolOR = (\a->(\b->a true (b true false)))
testOR = do
    print "OR true true = "
    print $ printBool $ boolOR true true
    print "OR true false = "
    print $ printBool $ boolOR true false
    print "OR false true = "
    print $ printBool $ boolOR false true
    print "OR false false = "
    print $ printBool $ boolOR false false

-- ***********************************************************
-- Conditional IF in pure lambda calculus
-- Our if will take 3 parameters IF boolean onSuccess onFailure

-- Our IF is just the identity function. This works because our representation of true and false do all the work.
-- true returns 1st parameter, false returns 2nd
-- SO our ifCond is just syntatic sugar and not really needed i.e. we could just have Bool onSuccess onFailure
ifCond = (\a->a)

testIF = do
    print "ifCond true CORRECT-TRUE WRONG"
    print $ ifCond true "CORRECT-TRUE" "WRONG"
    print "ifCond false WRONG CORRECT-FALSE"
    print $ ifCond false "WRONG" "CORRECT-FALSE"


-- **************************************************************
-- Numbers in lambda calculus
-- Theses are coded as (Alonso) Church's numerals

zero = (\f->(\x->x))
one = (\f->(\x->f x))
two = (\f->(\x->f (f x)))
three = (\f->(\x->f (f (f x))))
four = (\f->(\x->f (f (f (f x)))))
five = (\f->(\x->f (f (f (f (f x))))))
six = (\f->(\x->f (f (f (f (f (f x)))))))
seven = (\f->(\x->f (f (f (f (f (f (f x))))))))
eight = (\f->(\x->f (f (f (f (f (f (f (f x)))))))))
nine = (\f->(\x->f (f (f (f (f (f (f (f (f x))))))))))
ten = numSUCC(nine)
eleven = numSUCC(ten)
twelve = numSUCC(eleven)

-- This will output results for us in human readable form
-- This is obviously not using pure lambda calculus BUT merely a way to visualise Church numerals

-- This was my first attempt at a print function
printNUM0 n = 
    n numIt0 "0"
numIt0 x = '1':x

-- This is my 2nd and better attempt.
-- This will translate Church's Numerals into actual integers
printNUM n = 
    n numIt 0
numIt x = 1 + x

-- *****************************************
-- Successor function
-- Don't over analyse the following. Beta-reduce inner (n f x) which results in original number n
-- Then we add an f to the front
numSUCC = \n->(\f->(\x->f (n f x)))

-- *******************************************
-- ADDITION
--
-- The big one. Once we have this everything else should follow
-- I can sort of figure this out if you Beta reduce on paper BUT atm I doubt I would have been able to come up with this myself
-- Points for understanding the below:
-- (m f x) is like an identity function for Numerals. It raises the numeral to the current level
-- (n f m) is replacing the x in the number n with the number m. This is addition
numADD = \n->(\m->(\f->(\x->n f (m f x))))

testADD = do
    print "3 + 2"
    print $ printNUM $ numADD three two
    print "10 + 11"
    print $ printNUM $ numADD ten eleven

-- ******************************************
-- Multiplication
-- Got it from http://math.stackexchange.com/questions/595518/how-to-multiply-in-lambda-calculus
-- BUT I do not understand it

--numMULT = \n->(\m->(\f->(\x->(n (m f) x))))
-- This next is equivalent to the above
numMULT = \n->(\m->(\f->(n (m f))))

-- Doing it explicitly possibly makes it a little easier to understand
testMult1 = three (nine numIt) 0

testMULT = do
    print "3 * 2"
    let 
        res = numMULT three two 
        res2 = numMULT eleven res
        res3 = numMULT twelve res2
        res4 = numMULT twelve res3
        res5 = numMULT twelve res4
        res6 = numMULT twelve res5
        -- Next one blows the stack on my ghci environment
        --res7 = numMULT twelve res6
         in do
            print $ printNUM $ res
            print $ printNUM $ res2
            print $ printNUM $ res3
            print $ printNUM $ res4
            print $ printNUM $ res5
            print $ printNUM $ res6
            --print $ printNUM $ res7


-- ************************************************
-- RECURSION
-- Ability to loop is essential in a programming language (especially a Turing complete one)
-- In lambda calculus and pure functional languages this is achieved by recursion
--
-- Problem is it is not valid to have a lambda expression that recursively refers to itself. This rule is relaxed in most FP languages, such as haskell.
--
-- To understand how you do recursion in lambda calculus we will define a factorial lambda expression

-- First of all though we need two other building block function isZero and numPRED
-- From wikipedia: λn.n (λx.FALSE) TRUE
-- Actuallu once you've seen it it is quite easy to understand how this works
-- only zero returns its 2nd argument directly, all other Church numerals apply the function a number of times to the first argument
-- because our function always returns false then voila!
isZero = \n->(n (\x->false) true)

-- Now this one looks like a beast and wikipedia says it is complex
-- From wikipedia: λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)
-- Good luck trying to work this one out
numPRED = \n->(\f->(\x->(n (\g->(\h->h (g f))) (\u->x)(\u->u))))

testPRED = do
    print $ printNUM $ numPRED zero -- equals zero for this pred
    print $ printNUM $ numPRED twelve


-- The lambda expression we want is something like:
-- fact = \n->(ifCond (isZero n) one (numMULT n (fact (numPRED n))))
-- BUT this is not  a legal lambda expression because of the recursive reference to fact

-- ******************************************************
-- Y-Combinator
-- To allow recursion for nameless lambda functions we need what is called the Y-Combinator
-- λf.(λx.f (x x)) (λx.f (x x))
--
-- The haskell type system does not allow for this though. So we have to override type checking

yComb :: (a -> a) -> a
yComb = \f -> (\x -> f (unsafeCoerce x x)) (\x -> f (unsafeCoerce x x))

-- fact = yComb (\f->(\n->(ifCond (isZero n) one (numMULT n (f (numPRED n))))))
--fact = yComb (\f->(\n->(ifCond (isZero n) one (numMULT n (f (numPRED three))))))

fact :: Int -> Int
fact = fix (\f n -> if n == 0 then 1 else n * (f (n-1)))

-- I can't get the version of fact to compile that uses Chursch's Numerals.
-- Need to bring an end to this now. I am wasting too much time on this
--fact2 :: ((t -> t1) -> t -> t1) -> (t -> t1) -> t -> t1
-- fact2 = fix (\f n -> ifCond (isZero n) one (numMULT n (f (numPRED n))))


    