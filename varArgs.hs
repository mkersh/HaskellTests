-- Little experiment with defining a haskell function that can take var args parameter

-- I can't do what I was thinking about. Wanted to return a function that keeps on eating parameters up

func::Integer->Integer->(Integer->Integer)
func res 99 = (\y->res)
func acc x =
    (\y->if y == 99 then acc else acc+x+y)

-- eat:: Show a => a -> IO ()
eat::Integer->(Integer->Integer)
eat x = 
    (\y->x+y) 