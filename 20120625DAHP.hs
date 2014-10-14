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
    row2str i = (concat $ map (\j -> (",  "++(show (a!(i,j))))) js) ++ "\n"
    ((i0,j0), (m, n)) = bounds a
    is = [i0..m]
    js = [j0..n]

printMatrix :: (Show a)=> Matrix a -> IO()
printMatrix m = putStr $ matrix2str m


-- SPCM

isZero :: ElemType -> Bool
isZero t = (abs t) < epsillon

epsillon = 0.00001


bar :: Matrix ElemType -> Matrix ElemType
bar vec = array (bounds vec) [((i,j), (f (vec ! (i,j)))) | (i,j) <- indices vec]
  where
    f x
      | isZero x  = 0
      | otherwise = 1.0 / x

r :: (Int, Int) -> (Int, Int) -> ElemType
--r (c, a) (c', a')
r (1,1) (1,2) = 1/3
r (1,1) (1,3) = 5.0
r (1,2) (1,1) = 3.0
r (1,2) (1,3) = 3.0
r (1,3) (1,1) = 1/5
r (1,3) (1,2) = 1/3

r (2,1) (2,2) = 7.0
r (2,1) (2,3) = 3.0
r (2,2) (2,1) = 1/7
r (2,2) (2,3) = 1/3
r (2,3) (2,1) = 1/3
r (2,3) (2,2) = 3.0

r (3,1) (3,2) = 1/3
r (3,1) (3,3) = 1/3
r (3,2) (3,1) = 3.0
r (3,2) (3,3) = 1/3
r (3,3) (3,1) = 3.0
r (3,3) (3,2) = 3.0

r (4,1) (4,2) = 3.0
r (4,1) (4,3) = 5.0
r (4,2) (4,1) = 1/3
r (4,2) (4,3) = 1.0
r (4,3) (4,1) = 1/5
r (4,3) (4,2) = 1.0

r (1,1) (2,1) = 1/3
r (1,1) (3,1) = 3.0
r (1,1) (4,1) = 1/3

r (2,1) (1,1) = 3.0
r (2,1) (3,1) = 3.0
r (2,1) (4,1) = 1.0

r (3,1) (1,1) = 1/3
r (3,1) (2,1) = 1/3
r (3,1) (4,1) = 1/3

r (4,1) (1,1) = 3.0
r (4,1) (2,1) = 1.0
r (4,1) (3,1) = 3.0

r (1,1) (1,1) = 7.0
r (1,2) (1,2) = 10.0
r (1,3) (1,3) = 10.0
r (2,1) (2,1) = 7.0
r (2,2) (2,2) = 10.0
r (2,3) (2,3) = 10.0
r (3,1) (3,1) = 7.0
r (3,2) (3,2) = 10.0
r (3,3) (3,3) = 10.0
r (4,1) (4,1) = 7.0
r (4,2) (4,2) = 10.0
r (4,3) (4,3) = 10.0

r (c, a) (c', a')
 | (c==c') && (a==a') = 1.0
 | otherwise = 0


listC = [1,2,3,4]
listA = [1,2,3]

ca2i :: (Int, Int) -> Int
ca2i (c, a) = (c-1)*(length listA) + a

i2ca :: Int -> (Int, Int)
i2ca i = (c, a)
  where
    c = (div (i-1) (length listA)) + 1
    a' = (mod i (length listA))
    a = if a' == 0 then (length listA) else a'

matspcmNew :: Matrix ElemType
matspcmNew = array ((1,1), (ca, ca)) [((i, j), (f i j)) | i <- [1..ca], j<-[1..ca]]
  where
    ca = (length listC) * (length listA)
    f i j = rNew (ci,ai) (cj,aj)
      where
        (ci, ai) = i2ca i
        (cj, aj) = i2ca j
    rNew (ci, ai) (cj, aj)
      | (ci==cj) && (ai==aj) = 1.0
      | (ai==aj) && (ai==1) = r (ci,ai) (cj, aj)
      | (ai==aj) = (r (ci,ai) (ci, 1)) * (r (ci,1) (cj,1)) * (r  (cj,1) (cj,aj))
      | otherwise = r (ci,ai) (cj, aj)


matspcmNewCheck :: Matrix ElemType
matspcmNewCheck = array ((1,1), (ca, ca)) [((i, j), (f i j)) | i <- [1..ca], j<-[1..ca]]
  where
    ca = (length listC) * (length listA)
    f i j = rNew (ci,ai) (cj,aj)
      where
        (ci, ai) = i2ca i
        (cj, aj) = i2ca j
    rNew (ci, ai) (cj, aj)
      | (ci==cj) && (ai==aj) = 1.0
      | (ci==cj) = r2 (ci,ai) (cj, aj)
      | (ai==aj) = r2 (ci,ai) (cj, aj)
      | otherwise = 0.0
     
r2 :: (Int, Int) -> (Int, Int) -> ElemType
r2 ca1 ca2 = (r3 ca1)/(r3 ca2)

r3 :: (Int, Int) -> ElemType
r3 i = fromIntegral $ ca2i i

checkVec :: Matrix ElemType
checkVec = array ((1,1), (ca, 1)) [((i, 1), (r3 (i2ca i))) | i <- [1..ca]]
  where
    ca = (length listC) * (length listA)


matspcmOrg :: Matrix ElemType
matspcmOrg = array ((1,1), (ca, ca)) [((i, j), (f i j)) | i <- [1..ca], j<-[1..ca]]
  where
    ca = (length listC) * (length listA)
    f i j = r (ci,ai) (cj,aj)
      where
        (ci, ai) = i2ca i
        (cj, aj) = i2ca j

eigenMethod :: Int -> Matrix ElemType -> Matrix ElemType
eigenMethod n mat = eigenMethod' n mat $ array ((1,1), (ca,1)) [((i,1), (f i)) | i <- [1..ca]]
  where
    ca = (length listC) * (length listA)
--    f i = if i==1 then 1.0 else 0.0
    f i = 1.0
    

eigenMethod' :: Int -> Matrix ElemType -> Matrix ElemType -> Matrix ElemType
eigenMethod' 0 _ vec = vec
eigenMethod' n mat vec = eigenMethod' (n-1) mat $ normalize $ multiMatrix mat vec

spcmEvaluateMatrix :: Matrix ElemType -> Matrix ElemType
spcmEvaluateMatrix vec = array ((1,1), (length listA, length listC)) [((i, j), (f i j)) | i <- [1..(length listA)], j<-[1..(length listC)]]
  where
    f i j = vec!(ca2i(j,i), 1)/denom
    denom = sum [ vec!(ca2i (1, i), 1) |  i <- [1..(length listC)]]

sumEachRow :: Matrix ElemType -> Matrix ElemType
sumEachRow mat = array ((1,1), ((length listA),1)) [((a,1), f a) | a <- listA]
  where
   f a = f' a/(f' 1)
   f' a = sum [mat!(a,c)| c <- listC]

matEachDiv :: Matrix ElemType -> Matrix ElemType -> Matrix ElemType
matEachDiv matA matB= array (bounds matA) [(i, (matA!i)/(matB!i)) | i <- (indices matA)]

matZero ::Int -> Int -> Matrix ElemType
matZero m n = array ((1,1), (m,n)) [((i,j), 0.0) | i<-[1..m], j<-[1..n]]

matP :: Int -> Int -> Matrix ElemType
matP n index = array ((1,1), (n,n)) [((i,j), f i j)| i<-[1 .. n], j<-[1 .. n]]
 where
   f i j = if (i==j)&&(i==index) then 1.0 else 0.0

matListSum ::  Matrix ElemType -> [Matrix ElemType] -> Matrix ElemType
matListSum init xs = foldl addMatrix init xs



---- 
--- eval method 
---
-- printMatrix $ normalizeEachColumn $ eMethod 10 testEA

testEA = newMatrix [[1.0, 3.0, 7.0], [(1.0/3.0), 1.0, 3.0], [(1.0/7.0), (1.0/3.0), 1.0]] :: Matrix ElemType
testEA5 = newMatrix [[1.0, 3.0, 3.0, 5.0, 7.0], [(1.0/3.0), 1.0, 3.0, 5.0, 3.0], [(1.0/3.0), (1.0/3.0), 1.0, 3.0, 5.0], [(1.0/5.0), (1.0/3.0), (1.0/3.0), 1.0, 5.0], [(1.0/7.0), (1.0/5.0), (1.0/5.0), (1.0/5.0), 1.0]] :: Matrix ElemType

multiScalarMat :: ElemType -> Matrix ElemType -> Matrix ElemType
multiScalarMat c mat = array (bounds mat) [((i,j), (c * (mat ! (i,j)))) | (i,j) <- indices mat]

transpose :: Matrix a -> Matrix a
transpose a = array ((1,1), (n,m)) [((j,i), a!(i,j))| (i,j)<-indices a]
  where
     ((1,1), (m,n)) = bounds a

eMethod :: Integer -> Matrix ElemType -> Matrix ElemType
eMethod 0 mat = mat
eMethod n mat = eMethod (n-1) $ multiScalarMat (1.0/(fromIntegral size)) $ multiMatrix mat $ bar $ transpose mat
  where
    ((1,1), (size,_)) = bounds mat

{-
eMethod :: Integer -> Matrix ElemType -> Matrix ElemType
eMethod 0 mat = mat
eMethod n mat = eMethod (n-1) $ multiScalarMat (1.0/(fromIntegral size)) $ multiMatrix mat mat
  where
    ((1,1), (size,_)) = bounds mat
-}

normalizeEachColumn :: Matrix ElemType -> Matrix ElemType
normalizeEachColumn mat = array (bounds mat) [((i,j), (f j (mat!(i,j))))| (i,j)<-(indices mat)]
  where
    f j val = val / (den j)
    den j = sum [(mat!(i,k))| (i,k)<-(indices mat), k==j]
