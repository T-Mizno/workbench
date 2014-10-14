import Text.Regex.Posix
import Data.List
import System.Environment


repPatterns :: [String] -> [String] -> [(String, [String])]
-- rePatterns ids postPatterns
repPatterns [] _ = []
repPatterns (_id:_ids) _ps = (_id, map (_id ++) _ps) : repPatterns _ids _ps

each_check :: ([String], [String], [String]) -> ([String], [String])
-- each_check repPatterns remains bools
each_check ([], xs,  bs) = (bs, xs)
each_check (xs, [],  bs) = (bs, [])
each_check ( (x:xs), ys, bs) = 
      let t = _split (_match x) ys 
      in each_check (xs, (snd t), bs ++ [(fst t)])

_split :: (String -> Bool) -> [String] -> (String, [String])
--_split f xs = ( (show(filter f xs)), _notFilter f xs)
_split f xs = ( (flgExist (filter f xs)), _notFilter f xs)

_match :: String -> String -> Bool
_match pattern str = str =~ ("^" ++ pattern ++ "$")

_notFilter :: (a -> Bool) -> [a] -> [a]
_notFilter _ [] = []
_notFilter f (x:xs) = if (f x) then (_notFilter f xs) else (x : (_notFilter f xs))

check_loop :: [(String, [String])] -> [String] -> [(String, ([String], [String]))]
check_loop [] ss = [("last", (["Fail"], ss))]
check_loop (x:xs) ss = 
      ( (fst x), (each_check ((snd x), (filter (isPrefixOf (fst x)) ss), []))) : (check_loop xs (_notFilter (isPrefixOf (fst x)) ss) ) 

file2Strings :: String -> IO [String]
file2Strings filename = do
    str <- readFile filename
    return (_notFilter null (lines str))


flgExist :: [a] -> String
flgExist xs =   if null xs  then "" else "1"

_strs2str :: [String] -> String
_strs2str [] = ""
_strs2str (x:xs) = (flgExist x) ++ "," ++ _strs2str xs

strsShow :: [String] -> String
strsShow x = "\"" ++ (_strsShow x) ++ "\""

_strsShow :: [String] -> String
_strsShow (x:xs) =  x ++ " - " ++ (_strsShow xs)
_strsShow [] = ""

strsShowCol :: [String] -> String
strsShowCol (x:xs) = "\""++ x ++ "\"" ++ "\n" ++ (strsShowCol xs)
strsShowCol [] = ""

data2cvs :: [(String, ([String], [String]))] -> String
data2cvs [] = ""
data2cvs (("last", (_, ys)):_) = "wrongIDs:\n" ++ (strsShowCol ys)
data2cvs ((_id, (xs, [])):zs) =  "'"++_id ++","++ (_strs2str  xs)++ "\n" ++ data2cvs zs
data2cvs ((_id, (xs, ys)):zs) =  "'"++_id ++","++ (_strs2str  xs)++(strsShow ys) ++ "\n" ++ data2cvs zs

header2str :: [String] -> String
-- header2str patterns
header2str [] = "wrongFilename"
header2str (x:xs) = x ++ ", " ++ (header2str xs)

-- ghc --make check.hs
-- check testids.log testpostregs.log testSubmitted.log > test.csv
main = do
   args <- getArgs
   ids <- file2Strings (args !! 0)
   postPatterns <- file2Strings (args !! 1)
   submitted <- file2Strings (args !! 2)
--   putStrLn "IDs"
--   mapM_ (putStrLn.show) ids
--   putStrLn "Patterns"
--   mapM_ (putStrLn.show) postPatterns
--   putStrLn "submitted"
--   mapM_ (putStrLn.show) submitted
--   putStrLn "start"
--   mapM_ (putStrLn.show) (repPatterns ids postPatterns)
   let _data = (check_loop (repPatterns ids postPatterns) submitted)
--   mapM_ (putStrLn.show) _data
   putStrLn (", " ++ (header2str postPatterns))
   putStrLn (data2cvs _data)
