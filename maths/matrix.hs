{-
Just a quick experiment to see how quickly I can create a matrix multiplication function
-}
--import Data.Either -- already loaded in Prelude

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

matrix6 = [ [1, 1, 1, 1],
            [2, 2, 2, 2],
            [3, 3, 3, 3],
            [4, 4, 4, 4]]

matrix7 = [ [1, 2, 3, 4],
            [1, 2, 3, 4],
            [1, 2, 3, 4],
            [1, 2, 3, 4]]

matrix8 = [ [1, 0, 0, 0],
            [0, 1, 0, 0],
            [0, 0, 1, 0],
            [0, 0, 0, 1]]

matrix10 = [[1, 5, 9, 13],
            [2, 6, 10, 14],
            [3, 7, 11, 15],
            [4, 8, 12, 16]]

matrix11 = [ [1, 2, 3, 4],
             [1, 2, 3, 4]]

--matrixMULT m n =
transpose m = 
    transposeAux m 0 (length m)

transposeAux _ i n
    | i == n = []

transposeAux m i n =
    (map (getElements i 0) m):transposeAux m (i+1) n
    

getElements i c (mx:mxs) = 
    if i == c then mx else (getElements i (c+1) mxs)


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

-- Use this next one to test
matrixMULTStr n m = 
    multAllRows (singleRowBStr) n (transpose m)

-- This is the real matrix multiplication function
matrixMULT :: Num t => [[t]] -> [[t]] -> Either String [[t]]
matrixMULT n m = 
    let valid = checkSizes n m in
        if valid then Right (multAllRows (singleRowB) n (transpose m)) else Left "Array sizes incompatible!!!!"

checkSizes (ax:_) b = if length(ax) /= length(b) then False else True

-- We will make multAllRows and singleRowA generic so that it can work on matrices with different types of elements
multAllRows f [] _ = []
multAllRows f (ax:axs) b = 
    (singleRowA f ax b):(multAllRows f axs b) 

singleRowA f _ [] = []
singleRowA f aRow (bx:bxs) = 
    (f aRow bx):(singleRowA f aRow bxs)

-- This next one is a test function to concatenate the contents of matrix cells together. Aim is to allow you to easily see and test that it is working
singleRowBStr aRow bRow = 
    let tmpLst = zipWith (++) aRow bRow
        item = foldr (++) [] tmpLst
        in item

-- This next one is the proper function for use when matrix contains numbers
singleRowB aRow bRow = 
    let tmpLst = zipWith (*) aRow bRow
        item = foldr (+) 0 tmpLst
        in item


    

