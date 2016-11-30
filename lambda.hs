{-
I have been following the lambda calculus course from Arizona State University https://www.youtube.com/watch?v=_kYGDJSm0gE.

These are experiments to try out some of the ideas in haskell.

Reminder: In pure lamda calculus lambda expression are made up from the following rules

E -> ID
E -> \x. E     -- This is a function definition aka an abstraction
E -> E E       -- This is a function application aka a function call
E -> (E)

-}

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





