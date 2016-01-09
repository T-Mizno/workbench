import Data.Array
import Data.List
import System.Random

import MyzLinear

--  ‡—ñ
makeAllPermutations :: Int -> [[Int]]
makeAllPermutations n = permutations [1..n]

ps2MatrixView :: [[Int]] -> Matrix
ps2MatrixView ps = newMatrix' 1 m 1 n [((i,j), f i j) | i<-[1..m], j<-[1..n]]
              where
                m = length $ ps!!0
                n = length ps
                f i j = fromIntegral $ (ps!!(j-1))!!(i-1)

diffFromWPermutations :: [Int] -> [[Int]] -> [((Int, Int), NumType)]
diffFromWPermutations _ [] = []
diffFromWPermutations (w:ws) (p:ps) = (f w p) ++ (diffFromWPermutations ws ps)
                      where
                        n = length p
                        f w p = foldr (++) [] $ map (\pivot -> (map (\ref -> ((p!!(pivot-1), p!!(ref-1)), fromIntegral w)) [(pivot+1)..n])) [1..(n-1)]

diffMatrix :: Int -> [Int] -> Matrix
diffMatrix n ws = accum (+) (newMatrix 1 n 1 n [0,0..]) $ diffFromWPermutations ws $ makeAllPermutations n

srMatrix :: Int -> [Int] -> Matrix
srMatrix n ws = array (bounds dm) [((i,j),g i j) | i<-(rowIndices dm), j<-(colIndices dm)]
         where
                dm = diffMatrix n ws
                allN = length ws
                a = array (bounds dm) [((i,j),f i j) | i<-(rowIndices dm), j<-(colIndices dm)]
                f i j
                  | i/=j = if (dm!(i,j) * dm!(j,i)) < epsilon then 0.0 else dm!(i,j)/dm!(j,i)
                  | otherwise = 1.0
                g i j
                  | i/=j = a!(i,j)
                  | otherwise = 1.0 + (fromIntegral $ numOfZero i a)

numOfZero :: Int -> Matrix -> Int
numOfZero i a = length $ filter (\j -> (a!(i,j) < epsilon)) $ colIndices a

existZero :: Matrix -> Bool
existZero a = any (\(i,j) -> (a!(i,j) < epsilon)) $ indices a

realBolda :: [Int] -> [[Int]] -> Matrix
realBolda ws ps = accum (+) (newMatrix 1 m 1 1 [0,0..]) $ f ws ps []
          where
                m = (length (ps!!0))
                f :: [Int] -> [[Int]] -> [((Int,Int), NumType)] -> [((Int,Int), NumType)]
                f _ [] ls = ls
                f (w:sws) (p:sps) ls = f sws sps $ (map (\i -> (((p!!(i-1),1::Int)), fromIntegral $ w*(m-i+1))) [1..m]) ++ ls

realBolda' :: [Int] -> [[Int]] -> [((Int, Int), NumType)]
realBolda' ws ps =  f ws ps []
          where
                m = (length (ps!!0))
                f :: [Int] -> [[Int]] -> [((Int,Int), NumType)] -> [((Int,Int), NumType)]
                f _ [] ls = ls
                f (w:sws) (p:sps) ls = f sws sps $ (map (\i -> (((p!!(i-1),1::Int)), fromIntegral $ w*(m-i))) [1..m]) ++ ls


-- ‡—ñ‚ÍŠÖŒW‚È‚­Ž©—R‚É‚Â‚­‚é SRs—ñ
newSRMatrix :: Int -> Int -> Int -> Int -> [NumType] -> Matrix
newSRMatrix is ie js je vs = array (bounds base) [((i,j), f i j) | i<-[is..ie], j<-[js..je]]
            where
                base = newMatrix is ie js je vs
                f i j
                  | i==j = 1
                  | j>i = base!(i,j)
                  | otherwise = 1/(base!(j,i))

eigenVec :: Matrix -> Matrix
eigenVec a = x
         where
                (i, l, x, e) = powerMethod a 100

boldaVec :: Matrix -> Matrix
boldaVec a = array ( (iStart a, jStart a), (iEnd a, jStart a) ) [((i,(jStart a)), f i) | i <- (rowIndices a)]
         where
                f i =(sum $ map (\j -> a!(i,j)/(1+a!(i,j))) $ colIndices a) - 0.5

orderOfVec :: Matrix -> [Int]
orderOfVec a = sortBy (\i j -> compare (a!(j, (jStart a))) (a!(i, (jStart a)))) $ rowIndices a

sameOrder :: [Int] -> [Int] -> Bool
sameOrder is js = and $ map (\(i,j) -> i==j) $ zip is js

compVecs :: Matrix -> Bool
compVecs a = sameOrder (orderOfVec ex) (orderOfVec bx)
         where
                ex = eigenVec a
                bx = boldaVec a

-- ˆê‘Î”äŠrs—ñ‚Å•\Œ»‚Å‚«‚éƒ‚ƒm‚·‚×‚Ä‚É‚Â‚¢‚Ä‚Ì“¯’l«
check_vecs :: Int -> Int -> IO Bool
check_vecs seed n = do
  let rs = (randomRs (1, 10)(mkStdGen seed)) :: [NumType]
  let vals = take (n * n) rs
  let a = newSRMatrix 1 n 1 n $ vals
  let ex = eigenVec a
  let bx = boldaVec a
  let oe = orderOfVec ex
  let ob = orderOfVec bx
  let flg = sameOrder oe ob
  let flgt = (oe!!0) == (ob!!0)
  if (not flgt)
  then do
     print $ "seed : " ++ (show seed)
     print seed
     stdout a
     print oe
     stdout $ normalizeColumn ex (jStart ex)
     print ob
     stdout $ normalizeColumn bx (jStart bx) 
     return flg
   else
     return $  flg 
  
itr_check_vec :: Int -> Int -> Int -> IO()
itr_check_vec seed n itrMax = itr_seed_check' seed 0
   where
     itr_seed_check' :: Int -> Int -> IO()
     itr_seed_check' s itr = do
            flgOK <- check_vecs s n
            if (itr < itrMax) && flgOK then itr_seed_check' (s + 1) (itr + 1) else print "end"

-- ‡—ñ‚Å•\Œ»‚Å‚«‚éƒ‚ƒm‚É‚Â‚¢‚Ä‚Ì‚Ý‚Ì“¯’l«
check_vecs' :: Int -> Int -> IO Bool
check_vecs' seed n = do
  let rs = (randomRs (0, 3)(mkStdGen seed)) :: [Int]
  let ps = makeAllPermutations n
--  let ws = take (length ps) rs
  let ws = [6,0,0,5,2,0]
  let a = srMatrix n ws
  let ex = eigenVec a
  let bx = boldaVec a
  let rbx = realBolda ws ps
  let oe = orderOfVec ex
  let ob = orderOfVec bx
  let orb = orderOfVec rbx
  let flg = and [(sameOrder oe ob),  (sameOrder ob orb)]
--  let flgt = and [((oe!!0) == (ob!!0)),  ((ob!!0)==(orb!!0))]
  let flgt = and [((oe!!0) == (orb!!0))]
--  if (not flgt)
  if (True)
  then do
     print $ "seed : " ++ (show seed)
     print $ "ws : " ++  (show ws)
     print $ "sum of ws : " ++ (show $ sum ws)
     stdout $ ps2MatrixView ps
     stdout $ diffMatrix n ws
     stdout a
     print oe
     stdout $ normalizeColumn ex (jStart ex)
     print ob
     stdout $ normalizeColumn bx (jStart bx) 
     print orb
     stdout $ rbx
     stdout $ normalizeColumn rbx (jStart rbx)
     return flg
   else do
     stdout $ ps2MatrixView ps
     print ws
     stdout a
     return $  flg 
  
itr_check_vec' :: Int -> Int -> Int -> IO()
itr_check_vec' seed n itrMax = itr_seed_check' seed 0
   where
     itr_seed_check' :: Int -> Int -> IO()
     itr_seed_check' s itr = do
            flgOK <- check_vecs' s n
  --          if (itr < itrMax) && flgOK then itr_seed_check' (s + 1) (itr + 1) else print "end"
            if (itr < itrMax) then itr_seed_check' (s + 1) (itr + 1) else print "end"

