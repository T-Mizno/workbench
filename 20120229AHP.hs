import Data.Array

type Matrix a = Array (Int, Int) a
type ElemType = Double

testA22 = newMatrix [[0.768, 0.0879, 0.0751], [0.1576, 0.6694, 0.3332], [0.0816, 0.2426, 0.5917]] :: Matrix ElemType
testb22 = newMatrix [[0.4434], [0.3874], [0.1692]] :: Matrix ElemType

testA79 = newMatrix [[1.0, 0.1475, 0.1288], [0.2577, 1.0, 0.5678], [0.0969, 0.3577, 1.0], [0.2907, 0.9268, 0.0841]] :: Matrix ElemType
testb79 = newMatrix [[0.4783], [0.3723], [0.1495]] :: Matrix ElemType

testA86 = newMatrix [[1.0, 1.0], [3.0, 0.3], [5.0, 0.2]] :: Matrix ElemType
testb86 = (1, newMatrix [[0.25], [0.75]] :: Matrix ElemType)

testA100 = newMatrix [[1.0, 1.0], [3.0, 0.3], [5.0, 0.2]] :: Matrix ElemType
testb100_a1 = (1::Int, newMatrix[[0.187], [0.813]] :: Matrix ElemType)
testb100_a2 = (2::Int, newMatrix[[0.697], [0.303]] :: Matrix ElemType)
testb100_a3 = (3::Int, newMatrix[[0.852], [0.148]] :: Matrix ElemType)
testb100_is = [testb100_i1, testb100_i2, testb100_i3]
testb100_i1 = (1::Int, newMatrix[[0.25], [0.75]] :: Matrix ElemType)
testb100_i2 = (2::Int, newMatrix[[0.7], [0.3]] :: Matrix ElemType)
testb100_i3 = (3::Int, newMatrix[[0.8], [0.2]] :: Matrix ElemType)

testA101 = newMatrix [[1.0, 1.0], [1.5, 0.7], [2.5, 0.6]] :: Matrix ElemType
testb101_is = [testb101_1, testb101_2, testb101_3]
testb101_1 = (1::Int, newMatrix [[0.4], [0.6]] :: Matrix ElemType)
testb101_2 = (2::Int, newMatrix [[0.6], [0.4]] :: Matrix ElemType)
testb101_3 = (3::Int, newMatrix [[0.8], [0.2]] :: Matrix ElemType)


newMatrix :: [[ElemType]] -> Matrix ElemType
newMatrix xs = listArray ((1, 1), (m, n)) (concat xs)
  where
    m = length xs
    n = length (head xs)

culcCCM :: Int -> Matrix ElemType -> [(Int, Matrix ElemType)] -> [(Int, Matrix ElemType)]
culcCCM num a dbs 
 | num > 0  =  culcCCM (num - 1) a (loopOneCCM a dbs dbs)
 | otherwise = dbs

loopOneCCM a ((d, b):bs) dbs = (d, (normalize $ culcAverageCCM a d dbs)) : (loopOneCCM a bs dbs)
loopOneCCM a _ _ = []


culcAverageCCM a d ((i, b):bs) =  addMatrix (normalize $ multiMatrix (multiMatrix (diagonalMatrix d a) (diagonalInverseMatrix i a)) b) (culcAverageCCM a d bs)
culcAverageCCM a _ _ = (listArray ((ms,1), (me,1)) [0.0, 0.0 ..])
  where
    ((ms,_), (me,_)) = bounds a


aggregateScores :: Matrix ElemType -> Matrix ElemType -> Matrix ElemType
aggregateScores a b = normalize $ multiMatrix a b

aggregateScoresDominantAHP :: Matrix ElemType -> (Int, Matrix ElemType) -> Matrix ElemType
aggregateScoresDominantAHP a (d, b) = normalize $ multiMatrix (multiMatrix a (diagonalInverseMatrix d a)) b

diagonalMatrix :: Int -> Matrix ElemType -> Matrix ElemType
diagonalMatrix d a = (listArray ((ns, ns), (ne, ne)) [0.0, 0.0 ..])  // ([((j,j), a!(d,j))| j <- [ns..ne]])
  where
    ((_, ns), (_, ne)) = bounds a

diagonalInverseMatrix :: Int -> Matrix ElemType -> Matrix ElemType
diagonalInverseMatrix d a = (listArray ((ns, ns), (ne, ne)) [0.0, 0.0 ..])  // ([((j,j), 1/a!(d,j))| j <- [ns..ne]])
  where
    ((_, ns), (_, ne)) = bounds a


multiMatrix :: (Fractional a)=> Matrix a -> Matrix a -> Matrix a
multiMatrix a b = array ((1,1), (ma,nb)) assoc_ab
  where
    ((1,1), (ma, na)) = bounds a
    ((1,1), (mb, nb)) = bounds b
    assoc_ab = [((i,j), f i j) | i<-[1..ma], j<-[1..nb]]
    f i j = sum[a!(i,k) * b!(k,j)| k<-[1..mb]]

addMatrix :: (Num a) => Matrix a -> Matrix a -> Matrix a
addMatrix a b = array (bounds a) [((i,j), ((a!(i,j))+(b!(i,j)))) | (i,j) <- indices a]

denom :: Matrix ElemType -> ElemType
denom b = foldl (+) 0 $ elems b

normalize :: Matrix ElemType -> Matrix ElemType
normalize b = array (bounds b) [((i,j), ((b ! (i,j))/(denom b))) | (i,j) <- indices b]


matrix2str :: (Show a)=> Matrix a -> String
matrix2str a = concat $ map (\i -> (row2str i)) is
  where
    row2str i = (concat $ map (\j -> ("  "++(show (a!(i,j))))) js) ++ "\n"
    ((i0,j0), (m, n)) = bounds a
    is = [i0..m]
    js = [j0..n]

printMatrix :: (Show a)=> Matrix a -> IO()
printMatrix m = putStr $ matrix2str m
