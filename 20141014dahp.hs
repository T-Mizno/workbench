import MyzLinear
import Data.Array

w3 = newMatrix 1 3 1 3 [
   1, 1, 5,
   1, 1, 4,
   1/5, 1/4, 1]

(iw3, dw3, b3, err3) = powerMethod w3 100

wc1 = newMatrix 1 3 1 3 [
    1, 5, 9,
    1/5, 1, 2,
    1/9, 1/2, 1]

(i1, d1, a1, e1) = powerMethod wc1 100

wc2 = newMatrix 1 3 1 3[
    1, 1/7, 1/3,
    7, 1, 3,
    3, 1/3, 1]

(i2, d2, a2, e2) = powerMethod wc2 100

wc3 = newMatrix 1 3 1 3[
    1, 1/5, 1/7,
    5, 1, 1/2,
    7, 2, 1]
(i3, d3, a3, e3) = powerMethod wc3 100

ci l n = (l - (fromIntegral n))/((fromIntegral n) - 1.0)

a = newMatrix 1 3 1 3 [
  0.761, 0.088, 0.075,
  0.158, 0.669, 0.333,
  0.082, 0.243, 0.592]
  
a' = newMatrix 1 3 1 3 [
   8, 1/3, 1/8,
   2, 3, 1/2,
   1, 1, 1]

diag n = array (bounds a') [((i, j), f i j) | (i, j) <- (indices a')]
     where
        f i j
          | i == j  = a'!(n, j)
          | otherwise = 0

diag' n = array (bounds a') [((i, j), f i j) | (i, j) <- (indices a')]
     where
        f i j
          | i == j  = 1.0/a'!(n, j)
          | otherwise = 0

