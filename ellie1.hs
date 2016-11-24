
-- elies homework 24 Nov 2016
-- sum of the squares ofintegers  < 100 that are divisible by 3

theList = [3,6..]
list1 = take 100 theList    
list2 = [ x*x | x <- list1, x < 100]
