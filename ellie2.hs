f x = 2 / (4*(x^2)-1)

seq1 = [ f x | x<-[2,3..]]

seq2 = take 5000000 seq1

sum1 = foldr (+) 0 seq2
