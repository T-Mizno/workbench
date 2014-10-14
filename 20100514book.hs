{-- snippet RecursiveContents --}
-- module RecursiveContents (getRecursiveContents) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeExtension)
import Data.Char (toUpper)
import Data.List (isPrefixOf,nub)
import System.IO


getAllStr :: FilePath -> IO String
getAllStr topdir = do
  dirs <- getRecursiveContents2 topdir []
  _getAllStr "" dirs

_getAllStr :: String -> [FilePath] -> IO String
_getAllStr str (d:ds) = do
  str' <- dir2str d
  _getAllStr (str ++ str') ds
_getAllStr str [] = return str

getRecursiveContents :: FilePath -> IO [FilePath]

getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

getRecursiveContents2 :: FilePath -> [FilePath] -> IO [FilePath]

getRecursiveContents2 topdir ds = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents2 path (topdir:ds)
      else return [topdir]
  return (nub (concat paths))




data DirData = DirData {
      dirName :: String
      , contents :: String
    }

data ShortCut = ShortCut {
      shortCutPath :: FilePath
      , siteName :: String
      , url :: String
      }

dir2dirData :: FilePath -> IO DirData
dir2dirData dir = do
  scs <- getShortCuts dir
  let fs = map shortCut2Str scs
  return DirData {dirName=dir, contents=(foldl (++) ""  fs)}


dirData2str :: DirData -> IO String
dirData2str dir = do
  return ("<P><STRONG>"++(dirName dir)++"</STRONG><BR>" ++ (contents dir) ++"</P>")

dir2str :: FilePath -> IO String
dir2str dir = do
  dirData <- dir2dirData dir
  dirData2str dirData

urlSuffix = ".URL" :: String

getShortCuts :: FilePath -> IO [ShortCut]

getShortCuts dir = do
  names <- getDirectoryContents dir
  let shortCutNames = filter (\p -> (map toUpper (takeExtension p) == urlSuffix)) names
  paths <- forM shortCutNames $ \name -> do
    let path = dir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then return []
      else do
        _url <- extractURL path
        return [ShortCut{shortCutPath=path, siteName=(primeName name), url=_url}]
  return (concat paths)



urlKeyword = "URL=" :: String

extractURL :: FilePath -> IO String

extractURL path = do
  str <- readFile path
  let ls = filter (isPrefixOf urlKeyword) (splitLines str)
  return (drop (length urlKeyword) (myHead ls))
  
myHead :: [String] -> String
myHead (x:_) = x
myHead _ = ""



splitLines :: String -> [String]

splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of 
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerminator c = c == '\r' || c == '\n'
{-- /snippet splitLines --}


shortCut2Str :: ShortCut -> String
shortCut2Str sc = 
    "<A href=\"" ++ (url sc) ++ "\" target=_blank>" ++(siteName sc)++"</A><BR>\n" 
--                      ++ (shortCutPath sc) ++ "<BR>\n"

primeName :: String -> String
primeName str = take ((length str)-(length urlSuffix)) str


main = do
  str <- getAllStr "book"
--  str <- dir2str "book2"
  writeFile "test.html" str



