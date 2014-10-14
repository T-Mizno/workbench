import Text.ParserCombinators.Parsec
import System.Random
import Control.Monad (liftM)
import List (elemIndex)
import IO hiding (try)

data PSSData = PSSData{ index :: [Int],  qs :: [String],  as :: [[String]], base::[[String]] }

parsePSSData :: String -> PSSData
parsePSSData str = case (parseCSV str) of
              Right (l0:l1:l2:l3:lis)  -> PSSData [0..((length lis)-1)] (getQs lis) (getAs lis) (l0:l1:l2:l3:lis)
              otherwise             ->   PSSData [] [] [] []
  where
       getQs [] = []
       getQs ((_id:_q:_as):xs) = _q : (getQs xs)
       getQs (_:xs) = getQs xs
       getAs [] = []
       getAs ((_id:_q:_as):xs) =  _as: (getAs xs)
       getAs (_:xs) = getAs xs

choiceNum = 4

-- 整列しているリストを入力してデタラメな順序のリストを出力する
--  qsort の逆を行う
shuffleList :: [a] -> IO [a]
shuffleList [] = return []
shuffleList lis = do
   i0 <- getStdRandom(randomR(0, ((length lis)-1)))
   let (xs0, ys0) = splitAt i0 lis
   i <- getStdRandom(randomR(0, ((length lis)-1)))
   let (xs, ys) = splitAt i (ys0++xs0)
   xs' <- shuffleList (_tail xs)
   ys' <- shuffleList (_tail ys)
   return $ (_head ys) ++(_head xs) ++ ys' ++ xs'
      where
         _head [] = []
         _head x = [head x]
         _tail [] = []
         _tail x = tail x

-- i番目の要素を除いたリストからn個の要素をデタラメに抽出する
--  (リストの先頭は0番目)
pickup :: Int -> Int -> [a] -> IO [a]
pickup i n xs = do
  ps <- liftM (take n) $ shuffleList $ [p| p<-[0..((length xs) -1)], (p /= i)]
  return [(!!) xs p | p <- ps]

-- リストのデタラメな場所に要素を挿入したリストを返す
-- 挿入した場所iも返す (i, xs)
insertList :: a -> [a] -> IO (Int, [a])
insertList x xs = do
  ps <- shuffleList $ [0 .. (length xs)]
  let xs' = [(!!) (x:xs) p | p <- ps]
  case (elemIndex 0 ps) of
    Just i   -> return (i, xs')
    Nothing -> return ((-1), xs')

myMember :: (Eq a) =>  a -> [a] -> Bool
myMember x xs= or $ map (x == ) xs


main = do
  inh <- openFile "20100407DUO3.csv" ReadMode
  inStr <- hGetContents inh
  let d = parsePSSData inStr
  is <- shuffleList $ [0..((length (qs d))-1)]
  mainLoop is d
  hClose inh



mainLoop [] d = do
   putStrLn "Bye!"
mainLoop (i:is) d = do
  cs0 <- pickup i choiceNum (index d)
  (iInCs, cs) <- insertList i cs0
  putStrLn $ "\n\nQuestion: " ++ (show ((!!) (qs d) i))
  printChoices cs d
  putStrLn $ "      Answer is  " ++ (show iInCs) ++"("++(show i)++")"
  qLoop iInCs
  mainLoop is d

qLoop rightKey = do
  putStr "Input key ==>"
  inKey <- getChar
  let mKey = char2Int inKey
  case mKey of
    Nothing  -> qLoop rightKey
    Just key  -> if (key == rightKey) then putStrLn "OK" else putStrLn "worng" >> qLoop rightKey

char2Int :: Char -> Maybe Int
char2Int c
  |  myMember c "0123456789"  = elemIndex c "0123456789"
  |  otherwise = Nothing

printChoices [] d = putStrLn "--"
printChoices (c:cs) d = do
  putStrLn $ (show c) ++ "  " ++ (show ((!!) (as d) c))
  printChoices cs d

{-- from RWH --}

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell = 
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

{-- /snippet all --}
