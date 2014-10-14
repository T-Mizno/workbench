import MyzLinear
import Data.Array


-- ˆêŒËŒš‚Ä‚Æƒ}ƒ“ƒVƒ‡ƒ“‚Ìˆê‘Î”äŠr•\
cOfTop_58 = newMatrix 1 2 1 2 [
          1, 3/2,
          2/3, 1]

-- ˆêŒËŒš‚Ä‚ÌŠe•]‰¿Šî€“¯Žm‚Ìˆê‘Î”äŠr•\
cOfCI_58 = newMatrix 0 3 0 3 [
         1, 1/3, 1, 1/5,
         3, 1, 5, 1/2,
         1, 1/5, 1, 1/3,
         5, 2, 3, 1]
-- ƒ}ƒ“ƒVƒ‡ƒ“‚ÌŠe•]‰¿Šî€“¯Žm‚Ìˆê‘Î”äŠr•\
cOfCM_58 = newMatrix 0 3 0 3[
         1, 3, 1, 3,
         1/3, 1, 1/2, 1,
         1, 2, 1, 3,
         1/3, 1, 1/3, 1]

c2w :: Matrix -> Matrix
c2w c = w
  where
    (itr, l, w, d) = powerMethod c 100


-- Š—vŽžŠÔ‚Ì•]‰¿
eOfC1_59 = normalizeColumn (newMatrix 1 3 1 1 [ 1.0/1.5, 1.0/0.5, 1.0/1.0]) 1

-- Š—vŽžŠÔ‚Ìˆê‘Î”äŠr•\
cOfC1_59 = newMatrix 1 3 1 3 [
         1, 1/3, 1,
         3, 1, 2,
         1, 1/2, 1]

-- •¨Œ“à—e‚Ìˆê‘Î”äŠr•\
cOfC2_59 =  newMatrix 1 3 1 3 [
       1, 3, 5,
       1/3, 1, 3,
       1/5, 1/3, 1]

-- ”ƒ‚¢•¨‚Ì•Ö‚Ìˆê‘Î”äŠr•\
cOfC3_59 =  newMatrix 1 3 1 3 [
        1, 3, 1/5,
        1/3, 1, 1/7,
        5, 7, 1]

-- ŠÂ‹«‚Ìˆê‘Î”äŠr•\
cOfC4_59 = newMatrix 1 3 1 3 [
        1, 3, 1/4,
        1/3, 1, 1/5,
        4, 5, 1]

-- Š—vŽžŠÔ‚Ì•]‰¿
eOfC1_60 = normalizeColumn (newMatrix 3 3 1 1 [ 1.0/1.2, 1.0/0.2, 1.0/0.8]) 1

-- Š—vŽžŠÔ‚Ìˆê‘Î”äŠr•\
cOfC1_60 = newMatrix 1 3 1 3 [
         1, 1/6, 1,
         6, 1, 4,
         1, 1/4, 1]

-- •¨Œ“à—e‚Ìˆê‘Î”äŠr•\
cOfC2_60 = newMatrix 1 3 1 3 [
       1, 4, 3,
       1/4, 1, 1/2,
       1/3, 2, 1]

-- ”ƒ‚¢•¨‚Ì•Ö‚Ìˆê‘Î”äŠr•\
cOfC3_60 =  newMatrix 1 3 1 3 [
        1, 3, 1/2,
        1/3, 1, 1/3,
        2, 3, 1]

-- ŠÂ‹«‚Ìˆê‘Î”äŠr•\
cOfC4_60 = newMatrix 1 3 1 3 [
        1, 4, 2,
        1/4, 1, 1/3,
        1/2, 3, 1]

--matA_I = foldl (\mat1 mat2 -> bindMatrix mat1 mat2) eOfC1_59 [c2e cOfC2_59, c2e cOfC3_59, c2e cOfC4_59]
--matA_M = foldl (\mat1 mat2 -> bindMatrix mat1 mat2) eOfC1_60 [c2e cOfC2_60, c2e cOfC3_60, c2e cOfC4_60]

matA_I = foldl (\mat1 mat2 -> bindMatrix mat1 mat2) (c2e cOfC1_59) [c2e cOfC2_59, c2e cOfC3_59, c2e cOfC4_59]
matA_M = foldl (\mat1 mat2 -> bindMatrix mat1 mat2) (c2e cOfC1_60) [c2e cOfC2_60, c2e cOfC3_60, c2e cOfC4_60]


c2e :: Matrix -> Matrix
c2e c = e
  where
    (itr, l, e, d) = powerMethod c 100
        

w_I = c2w cOfCI_58
w_M = c2w cOfCM_58

w_Top = c2w cOfTop_58


bindMatrix :: Matrix -> Matrix -> Matrix
bindMatrix a b = newMatrix' (iStart a) (iEnd a) (jStart a) ((jEnd a) + (colSize b)) [((i,j), v i j) | i <- (rowIndices a), j <- [(jStart a) .. ((jEnd a) + (colSize b))]]
  where
    v i j
     | j <= (jEnd a)  = a!(i,j)
     | otherwise      = b!(i, (jStart b) + j - (jEnd a)-1)


ci :: Int -> NumType -> NumType
ci n l = (l - (fromIntegral n)) / (fromIntegral $ n - 1)


--  stdout $ multiScalar (w_Top!(1,0)) $ multi matA_I w_I
--  stdout $ multiScalar (w_Top!(2,0)) $ multi matA_M w_M