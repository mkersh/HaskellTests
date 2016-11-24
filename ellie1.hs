
-- elies homework 24 Nov 2016
-- sum of the squares ofintegers  < 100 that are divisible by 3

theList = [3,6..]
list1 = take 100 theList    
list2 = [ x*x | x <- list1, x < 100]

-- The mathematicl answer was Sum( (3r)^2) for r = 1 to r = 33
