import Text.ParserCombinators.Parsec  {- for CSV  from RWH ch16 -}

import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM, mapM)
import Text.Regex.Posix ((=~))
import Data.List (nub, (\\))

csv2str :: [[String]] -> String
csv2str strs = unlines $ map csvline2str strs

csvline2str :: [String] -> String
csvline2str [] = ""
csvline2str (x:[]) = x
csvline2str (x:xs) = x ++ ","++(csvline2str xs)

getContentsNotDot path = do
  fs <- getDirectoryContents path
  return $ fs \\ [".", ".."]

remains2str :: [(String, [String])] -> String
remains2str [] = ""
remains2str ((cls,strs):r) = cls ++ "--------\n"++ (_remains2str $ reverse strs) ++ (remains2str r)
  where
    _remains2str [] = "\n"
    _remains2str (s:ss) = s ++ "\n" ++ (_remains2str ss)

main = do
  listFileStr <- readFile "list.csv"
  prefixFileStr <- readFile "prefix.txt"
  let prefixs = filter (\s -> ((length s) > 0)) $ lines prefixFileStr
  print "postfix"
  print prefixs
  case parseCSV listFileStr  of
     Left e -> putStrLn "csv parse error" >> print e
     Right (rTitle:r) -> do
       resultCSV <- forM r $ \(no:cls:id:name:_) ->  do
           files <- getDirectoryContents $ ("." </>  cls)
           let ptns = map (id ++) prefixs
           let eachFlg = map (\p -> if (any (\f -> (f =~ p)) files) then "1" else "") ptns
           let eachResult = no:cls:("'"++id):name:eachFlg
           return eachResult
       writeFile "out.txt"  $ ((csvline2str (take 4 rTitle)) ++ ","++(csvline2str prefixs) ++ "\n") ++ (csv2str resultCSV)

       -- for remains
       let allRegs = concat $ map (\p -> map (\i -> i </> p) (map (\(_:cls:id:_) -> ("." </> cls </> id)) r)) prefixs
       let allDir = Data.List.nub $ map (\(_:c:_) -> c) r
       print allDir
       remains <- forM allDir $ \c -> do
            let eachClsMember = map (\(_:_:i:_) -> i) $ filter (\(_:aCls:id:_) -> (aCls == c)) r
            let eachAllPtns =  concat $ map (\i -> (map (i++) prefixs)) eachClsMember
            eachAllFiles <- getContentsNotDot $ ("." </> c)
            let eachOKs = concat $ map (\p -> filter (\f -> ((f =~ p) :: Bool)) eachAllFiles) eachAllPtns
            let eachNGs = eachAllFiles \\ eachOKs
            return (c, eachNGs)
       writeFile "remains.txt" $ remains2str remains

{-  from RWH ch16 -}
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

{-
main =
    do c <- getContents
       case parse csvFile "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> mapM_ print r
-}