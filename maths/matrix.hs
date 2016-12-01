{-
Just a quick experiment to see how quickly I can create a matrix multiplication function
-}

-- Initially we will just use nested lists to represent a matrix

matrix1 = [ [1.1, 1.2, 1.3, 1.4],
            [2.1, 2.2, 2.3, 2.4],
            [3.1, 3.2, 3.3, 3.4],
            [4.1, 4.2, 4.3, 4.4]]

matrix2 = [ [1.1, 1.2, 1.3, 1.4],
            [2.1, 2.2, 2.3, 2.4],
            [3.1, 3.2, 3.3, 3.4],
            [4.1, 4.2, 4.3, 4.4]]

matrix3 = [ [1.1, 1.2, 1.3, 1.4],
            [2.1, 2.2, 2.3, 2.4]]

matrix4 = [ ["a11", "a12"],
            ["a21", "a22"]]

matrix5 = [ ["b11", "b12"],
            ["b21", "b22"]]

--matrixMULT m n =
transpose m = 
    transposeAux m 0

transposeAux _ 2 = []

transposeAux m i =
    (map (getElements i 0) m):transposeAux m (i+1)
    

getElements i c (mx:mxs) = 
    if i == c then mx else (getElements i (c+1) mxs)

--matrixMULT n m = 
--    zipWith matrixMULT2 n (transpose m)

--matrixMULT2 r1 r2 = 
--    zipWith (++) r1 r2

-- Basic algorithm for multiplying two matrices A + B together together
--  B' is the transpose of B
-- 
-- MatrixRes = []
-- For each rowA in A
--      rowRes = []
--      For each rowB in B'
--          tmpLst = zipWith (*) rowA rowB
--          item = foldr (+) tmpLst
--          rowRes.append(item)
--      MatrixRes.append(rowRes) 

