
comb :: [a] -> Int -> [[a]]

comb []  _ = []
comb xs 0 = [[]]
comb (x:xs) n
  | (length (x:xs)) <= n   = [(x:xs)]
  | otherwise  =  (map (x:) $ comb xs (n-1))    ++  comb xs n


combWithRest :: [a] -> Int -> [([a], [a])]

combWithRest xs 0 = [([], xs)]
combWithRest [] _ = []
combWithRest (x:xs) n =
  [(x:ys, zs) | (ys, zs) <- combWithRest xs (n-1)] ++ [(ys, x:zs) | (ys, zs)<- combWithRest xs n]


nCr ::  (Integral a) =>  a -> a -> a

nCr _ 0  =  1
nCr n r
  |  n == r  = 1
  |  otherwise  = div ( (n-r+1) * nCr n  (r-1) )  r



-- “ñ€•ª•z
bini :: Double -> Integer -> Integer -> Double
bini p n x = (p ^ x) * ((1.0 - p) ^ (n - x)) * ( fromInteger (nCr n x))
