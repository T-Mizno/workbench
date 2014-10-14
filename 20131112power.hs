import MyzLinear

import Data.Array
import Data.List
import Debug.Trace

import System.Random
--import Control.Monad(forM)
--import Test.QuickCheck

powerMethod2 :: Matrix -> Int -> (Int, Matrix, NumType)
powerMethod2 a itrMax = searchFixedMatrix (\x -> normalizeColumn2 (axbar x) 0) xe itrMax
  where
    axbar vec = multi a nrVec
      where
        rVec = reciprocalizeMatrix vec
        nrVec = multiScalar (1.0 / ((norm2 rVec 0)^2)) rVec
    xe = filledMat (resVec a) 1.0

powerMethod4 :: Matrix -> Int -> (Int, Matrix, NumType)
powerMethod4 a itrMax = searchFixedMatrix (\x -> normalizeColumn2 (axbar x) 0) xe itrMax
  where
    axbar vec = sub term1 term2
      where
        term1 = multiScalar 2.0 $ multi a nrVec
--        term2 = multi ata $ multiScalar nbarDeno vec
        term2 = nrVec
--        maxElem vec = Data.List.maximum [vec!(i,0) | i <- (rowIndices vec)]
--        minElem vec = Data.List.minimum [vec!(i,0) | i <- (rowIndices vec)]
--        delta  = (minElem term1)/(maxElem term2)
--        delta  = (- 0.001)
        rVec = reciprocalizeMatrix vec
        nbarDeno = 1.0 / ((norm2 rVec 0)^2)
        nrVec = multiScalar nbarDeno rVec
--        nrVec = rVec
--        ata = multi (trans a) a
--        trAtA = sum [a!(i,j)^2 | (i,j) <- indices a]
--        trAtAdivN = trAtA/ (fromIntegral $ rowSize a)
--        trAtAdivN = 2.0
    xe = filledMat (resVec a) 1.0

powerMethod4''' :: Matrix -> Int -> (Int, Matrix, NumType)
powerMethod4''' a itrMax = searchFixedMatrix (\x -> normalizeColumn2 (axbar x) 0) xe itrMax
  where
--    axbar vec = add (add (sub (multi a nrVec) (multiScalar 0.01 nrVec)) (multi a vec)) (multiScalar 0.1 xe)
--    axbar vec = add (sub (multi a nrVec) (multiScalar 1.0 nrVec)) (multi a vec)
    axbar vec = add (add (multi a nrVec) (multiScalar (1.0/(fromIntegral $rowSize a)) nrVec)) (multiScalar (1.0/(fromIntegral $ rowSize a)) (multi a vec))
      where
        rVec = reciprocalizeMatrix vec
        nbarDeno = 1.0 / ((norm2 rVec 0)^2)
        nrVec = multiScalar nbarDeno rVec
        trAtA = sum [a!(i,j)^2 | (i,j) <- indices a]
        trAtAdivN = trAtA/ (fromIntegral $ rowSize a)
--        trAtAdivN = 2.0
    xe = filledMat (resVec a) 1.0

powerMethod4' :: Matrix -> Int -> (Int, Matrix, NumType)
powerMethod4' a itrMax = searchFixedMatrix (\x -> axbar x ) xe itrMax
  where
--    axbar vec = multi a $ normalizeColumn2 (reciprocalizeMatrix $ normalizeColumn2 vec 0) 0 
    axbar vec = multiScalar (1.0/(innerProduct nVec nVec)/(innerProduct rVec rVec)) $ multi a rVec
      where
        nVec = normalizeColumn2 vec 0
        rVec = multiScalar (1.0 / (fromIntegral $ rowSize a)) $ reciprocalizeMatrix nVec
--        nrVec = normalizeColumn2 rVec
--    (ie, le, xe, ee) = powerMethod a itrMax
    xe = resVec a

reciprocalizeMatrix :: Matrix -> Matrix
reciprocalizeMatrix m = array (bounds m) [ ((i,j), f $ m!(i,j)) | (i,j) <- (indices m)]
   where
     f x
      | isZero x = x
      | otherwise = 1.0 / x

powerMethod3' :: Matrix -> Int -> (Int, Matrix, NumType)
powerMethod3' a itrMax = searchFixedMatrixE (\x -> mkpair $ updatex x)  (mkpair $ xE''') itrMax
  where
    updatex x = normalizeColumn (add x $ multiScalar (0.01 / (fromIntegral $ length $ indices a))   $ normalizeColumn (delta x) 0) 0
    mkpair x = (x, matDiff2 a $ mkSR x)
    (itE, lE, xE, eE) = powerMethod a itrMax
    (itE2, xE2, eE2) = powerMethod2 a itrMax
    xE' = averageColumn a
    xE'' = multiScalar 0.5 $ add xE $ reciprocalizeMatrix xE2
    (itrr, xE''', re) = resVec2 a 100
    delta x = normalizeColumn (array (bounds x) [ ((k,0), (- er) * (partial k)) | k <- (rowIndices x)]) 0
      where
        partial k = sum [ (-2) * (a!(k,i) - x!(k,0) / x!(i,0)) / x!(i,0)
                          + 2 * (a!(i,k) - x!(i,0) / x!(k,0)) * x!(i,0) / ((x!(k,0))^2)
                           | i <- (rowIndices x) \\ [k] ]
        partial' k = sum [ (x!(k,0) / ((x!(i,0))^2))
                          - ((x!(i,0))^2) / ((x!(k,0))^3)
                          + (a!(i,k) * x!(i,0) / (x!(k,0))^2)
                          - (a!(k,i)/x!(i,0))
                           | i <- ((rowIndices x) \\ [k]) ]
        er = sum [ (a!(i,j) - (x!(i,0) / x!(j,0))) ^ 2 | (i,j) <- (indices a)]

powerMethod3 :: Matrix -> Int -> (Int, Matrix, NumType)
powerMethod3 a itrMax = (i1+i2, maxVec, maxE)
  where
     (i1, l1, x1', d1) = powerMethod a itrMax
     (i2, x2', d2) = powerMethod2 a itrMax
--     x1 = normalizeColumn2 ( reciprocalizeMatrix x1' ) 0
     x1 = normalizeColumn2 (filledMat x1' 1.0) 0
--     x1 = normalizeColumn2 x1' 0
     x2 = normalizeColumn2 x2' 0
     evs = map (\e -> (e, add (multiScalar (1.0 - e) x1) (multiScalar e x2))) $ [0.0, 1.0] ++ ( dtList 10.0 1000)
     (maxE, maxVec) = searchMax (head evs) (tail evs)
     searchMax :: (NumType, Matrix) -> [(NumType, Matrix)] -> (NumType, Matrix)
     searchMax ev [] = ev
     searchMax ev (ev1:aevs) =
        if  (matDiff2 a (mkSR (snd ev1))) < (matDiff2 a (mkSR (snd ev)))
        then searchMax ev1 aevs
        else searchMax ev aevs

powerMethod3''' :: Matrix -> Int -> (Int, Matrix, NumType)
powerMethod3''' a itrMax =(i1+i2+iad, multi matD $ normalizeColumn xad 0, xad!(2,0))
  where
     (i1, l1, x1', d1) = powerMethod a itrMax
     (i2, x2', d2) = powerMethod2 a itrMax
--     x0 = normalizeColumn2 (filledMat x1' 1.0) 0
     x1 = normalizeColumn2 x1' 0
     x2 = normalizeColumn2 x2' 0
     x0 = x2
     matD = newMatrix' (jStart a) (jEnd a) 0 2 [ ( (i, j), f i j) | i <- (colIndices a), j <- [0..2]]
         where
           f i j
            |  j == 0  = x0!(i,0)
            |  j == 1  = x1!(i,0)
            |  otherwise = x2!(i,0)
     matDAD = multi (trans matD) $ multi a matD
     (iad, lad, xad, dad) = powerMethod matDAD itrMax
--     (iad, xad, dad) = powerMethod2 matDAD itrMax

powerMethod3'' :: Matrix -> Int -> (Int, Matrix, NumType)
powerMethod3'' a itrMax = (i1+i2+iN, normalizeColumn (add (multiScalar maxE x1) (multiScalar (1.0 - maxE) x2)) 0, maxE)
  where
     (i1, l1, x1', d1) = powerMethod a itrMax
     (i2, x2', d2) = powerMethod2 a itrMax
     x1 = normalizeColumn2 (filledMat x1' 1.0) 0
     x2 = normalizeColumn2 x2' 0
     (iN, maxE) = newtonMethod (error x1 x2) (error' x1 x2) 0.5 itrMax
--     error x y e = sum [(a!(i,j) - (e * x!(i,0) + (1.0 - e) * y!(i,0))/(e * x!(j,0) + (1.0 - e) * y!(j,0))) ^ 2 | (i,j) <- (indices a)]
     error x y e = (matDiff2 a $ mkSR $ add (multiScalar e x) (multiScalar (1.0 - e) y)) ^ 2
     error' x y e = 
          (- 2.0) * sum [(a!(i,j) - (e * x!(i,0) + (1.0 - e) * y!(i,0))/(e * x!(j,0) + (1.0 - e) * y!(j,0)))
                          * ( ( (x!(i,0) - y!(i,0)) * ( e * (x!(j,0) - y!(j,0)) + y!(j,0)) - (e * (x!(i,0) - y!(i,0)) + y!(i,0)) * (x!(j,0) - y!(j,0)))
                               / ( (e * (x!(j,0) - y!(j,0)) + y!(j,0))^2 ))
                          | (i,j) <- (indices a)]

newtonMethod :: (NumType -> NumType) -> (NumType -> NumType) -> NumType -> Int -> (Int, NumType)
newtonMethod f f' x0 itrMax = newtonItr x0 0
  where
    newtonItr :: NumType -> Int -> (Int, NumType)
    newtonItr x itr
      |  isZero $ f x   =  (itr, x)
      |  itr >= itrMax  =  (itrMax, x)
      |  otherwise      = trace (show $ (sqrt $ f x, x)) $ newtonItr (x - (f x)/(f' x)) (itr + 1)

dtList :: NumType -> Int -> [NumType]
dtList border n = map (\i -> (fromIntegral i) * (2.0 * border / (fromIntegral n)) - border) [0..n]

resVec :: Matrix -> Matrix
resVec a = resVec0 a $ newMatrix (jStart a) (jEnd a) 0 0 [1,1..]

resVec2 :: Matrix -> Int -> (Int, Matrix, NumType)
--resVec2 a itrMax = searchFixedMatrix (\x -> resVec0 a x) (newMatrix (jStart a) (jEnd a) 0 0 [1,1..]) itrMax
resVec2 a itrMax = searchFixedMatrix (\x -> resVec0 a x) (averageColumn a) itrMax

resVec0 :: Matrix -> Matrix -> Matrix
resVec0 a x0 = normalizeColumn ( multi (srResolvent a z) x0 ) 0
  where
    z = ((fromIntegral $ colSize a) - 1) + 0.5

srResolvent :: Matrix -> NumType -> Matrix
srResolvent a z = array (bounds a) [((i,j), f i j) | (i,j) <- (indices a)]
  where
    f i j
     | i == j     = (z - (n - 1)) / ( z * (n - z))
     | otherwise  = a!(i,j) / (z * ( n - z))
    n = (fromIntegral $ colSize a)


rayleighQ :: Matrix -> Matrix -> Matrix -> NumType
rayleighQ y a x = (innerProduct y $ multi a x) / (innerProduct y y) / (innerProduct x x)

rayleighR :: Matrix -> Matrix -> NumType
rayleighR a x = rayleighQ rx a x/(innerProduct x x)/(innerProduct rx rx)
  where
    rx = (multiScalar (1.0 / (fromIntegral $ rowSize a)) (reciprocalizeMatrix x)) 
    

powerMethod4'' :: Matrix -> Int -> (Int, Matrix, NumType)
powerMethod4'' a itrMax = rayleighItr a q0 v0 0.0 0
  where
--    q0 = resVec a
    q0 = normalizeColumn2 (averageColumn a) 0
    v0 = rayleighR a q0
    rayleighItr :: Matrix -> Matrix -> NumType -> NumType -> Int -> (Int, Matrix, NumType)
    rayleighItr a q v preD itr
       | itr > itrMax  = (itr, q, d)
--       | near d preD = (itr, q', d)
--       | d > preD = (itr, q, d)
       | isZero (matDiff q q') = (itr, q', d)
       | otherwise = rayleighItr a q' v' d (itr + 1)
       where
         d = trace (show tmpE) $ matDiff q q'
         (_, _,_,_, _, z', _) = gauss a' q
         a' = array (bounds a) [ ((i,j), f i j) | (i,j) <- (indices a)]
         f i j
           | i == j = a!(i,j) - v
           | otherwise = a!(i,j)
         q' = normalizeColumn2 z' 0
         v' = rayleighR a q'
         tmpE = matDiff2 a (mkSR q')




--
-- sample
--
matSR = newMatrix 2 7 2 7 [
   1,   3,   1/2,  4,   6,   7,
   1/3, 1,   1/3,  5,   5,   6,
   2,   3,   1,    3,   4,   7,
   1/4, 1/5, 1/3,  1,   1,   2,
   1/6, 1/5, 1/4,  1,   1,   2,
   1/7, 1/6, 1/7,  1/2, 1/2, 1
   ]

ansSR = newMatrix 2 7 0 0 [ 0.300, 0.195, 0.346, 0.066, 0.058, 0.035]

mat79 = newMatrix 1 7 1 7 [
      1,   4,   9,   6,   6,   5,   5,
      1/4, 1,   7,   5,   5,   3,   4,
      1/9, 1/7, 1,   1/5, 1/5, 1/7, 1/5,
      1/6, 1/5, 5,   1,   1,   1/3, 1/3,
      1/6, 1/5, 5,   1,   1,   1/3, 1/3,
      1/5, 1/3, 7,   3,   3,   1,   2,
      1/5, 1/4, 5,   3,   3,   1/2, 1
      ]

matDiffVec :: Matrix -> Matrix -> NumType
matDiffVec a w = sum [(a!(i,j) * w!(j,0)  - w!(i,0))^2 | (i, j) <- (indices a)]

randomSRMatrix :: Int -> Int -> Matrix
randomSRMatrix n seed = array (bounds pre) [( (i,j), f i j) | (i,j) <- (indices pre)]
  where
     pre =  newMatrix 0 (0 + n -1 )  0 (0 + n -1 )  $ take ( n * n) $ map (\i -> vals!!(rem (abs i) (length vals))) ( (randoms (mkStdGen seed)) :: [Int])
     f i j
       |  i == j  = 1.0
       |  j > i   = pre!(i,j)
       | otherwise = 1.0 / (pre!(j,i))
     vals = concat [[1..30], (map (\x->(1.0/(fromIntegral x))) [1..30])]

randomIntScaleSRMatrix :: Int -> Int -> Matrix
randomIntScaleSRMatrix n seed = array (bounds pre) [( (i,j), f i j) | (i,j) <- (indices pre)]
  where
     pre =  newMatrix 0 (0 + n -1 )  0 (0 + n -1 )  $ take ( n * n) $ map (\i -> scales!!i) ( (randomRs (0, (length scales)-1) (mkStdGen seed)) :: [Int])
     f i j
       |  i == j  = 1.0
       |  j > i   = pre!(i,j)
       | otherwise = 1.0 / (pre!(j,i))
     scales = [(1/9), (1/7), (1/5), (1/3), 1, 3, 5, 7, 9] :: [NumType]

randomIntScaleSRMatrix' :: Int -> Int -> Matrix
randomIntScaleSRMatrix' n seed = array (bounds pre) [( (i,j), f i j) | (i,j) <- (indices pre)]
  where
     pre =  newMatrix 0 (0 + n -1 )  0 (0 + n -1 )  $ take ( n * n) $ map (\i -> (scales i)) ( (randomRs (1, 50) (mkStdGen seed)) :: [Int])
     f i j
       |  i == j  = 1.0
       |  j > i   = pre!(i,j)
       | otherwise = 1.0 / (pre!(j,i))
     scales :: Int -> NumType
     scales i 
       | 1 <= i && i <= 18  = 1.0
       | 19 <= i && i <= 25 = 1.0/3.0
       | 26 <= i && i <= 32 = 3.0
       | 33 <= i && i <= 37 = 1.0/5.0
       | 38 <= i && i <= 42 = 5.0
       | 43 <= i && i <= 45 = 1.0/7.0
       | 46 <= i && i <= 48 = 7.0
       | i == 49  = 1.0/9.0
       | i == 50  = 9.0
       | otherwise = 1.0

ciOfRandomIntScaleSRMatrix :: Int -> Int -> NumType
ciOfRandomIntScaleSRMatrix n seed = (l - (fromIntegral n))/((fromIntegral n) - 1.0)
  where
--        (i, l, x, e) = powerMethod (randomIntScaleSRMatrix n seed) 200
        (i, l, x, e) = powerMethod (randomIntScaleSRMatrix' n seed) 200

mkSR :: Matrix -> Matrix
mkSR x = multi x $ trans $ reciprocalizeMatrix x

comparePM :: Int -> Int -> String
comparePM n seed = trace (show $ (itr1, itr2, itr3, itr4, d1, d2, d3, d4)) $ minNum d1 d2 d3 d4
  where
    (itr1, l1, x1, e1) = powerMethod a 200
    (itr2, x2, e2) = powerMethod2 a 200
    (itr3, x3, e3) =  powerMethod3 a 200
    (itr4, x4, e4) =  powerMethod4 a 200
    d1 = matDiff2 a $ mkSR x1
    d2 = matDiff2 a $ mkSR x2
    d3 = matDiff2 a $ mkSR x3
    d4 = matDiff2 a $ mkSR x4
    a = randomSRMatrix  n  seed
    minNum v1 v2 v3 v4 = 
           if (v1 <= v2) && ( v1 <= v3) && ( v1 <= v4)
           then "1"
           else
                if (v2 <= v3) && (v2 <= v4)
                then "2"
                else  if (v3 <= v4) 
                      then "3"
                      else "4"


-- let a = randomSRMatrix  7 32
-- *Main> map (\e -> matDiff2 a $ mkSR $ add (multiScalar (1.0 - e) x1') (multiScalar e x2')) [-0.5, -0.01, 0.0, 0.00001, 0.001, 0.1]


--  map (\e -> (e, matDiff2 matSR $ mkSR $ add (multiScalar (1.0 - e) x1') (multiScalar e x2'))) $ dtList 10 100
-- mapM (\i -> putStrLn $ (show (fst i)) ++ ", " ++(show (snd i))) evs

main = mapM (\l -> print l) $ map (\s -> ciOfRandomIntScaleSRMatrix 5 s) [1..10000]