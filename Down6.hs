import Network.HTTP
import System.IO
import Data.Maybe
import Network.URI
import Text.Regex.Posix
import System.FilePath
import Data.Char (toLower)
import Data.List
import Data.List.Split (splitOn)
import GHC.IOBase
import System.Cmd
import Control.Concurrent

downFromURL :: String -> String -> IO [()]
downFromURL prefix url = do	
	eStr <- downloadURL url
	remains <- case eStr of
		Left errStr -> return $ errStr :[]
		Right str -> (wgetMain url prefix []) $ deleteDual $ (filter isNotThumb) $ (filter isImageURL) $ (map extractURL) $ getHrefURL str
	mapM print remains

downFromFile :: String -> String -> IO [()]
downFromFile prefix fileName = do
	iStr <- readFile fileName
	let urls = map confURL  (lines  iStr)
	remains <- wgetMain "" prefix [] urls
	print "Download Failue:"
	mapM print remains


wgetCmd :: String
wgetCmd = "./wget/wget"

wgetParam :: String -> String -> String -> [String]
wgetParam refURL url prefix = ["-r", "-l1", "--no-parent", "-A"++(takeExtension url), "--header=REFERER:"++refURL, "-U", "\"Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; .NET CLR 1.1.4322)\"", "-O"++prefix++(getFileName url), url, "-e", "robots=off"]

wgetMain :: String -> String -> [String] -> [String] -> IO [String]
wgetMain _ _ remains [] = return remains
wgetMain refURL prefix remains ("":us)  = wgetMain refURL prefix remains us
wgetMain _refURL prefix remains (u:us) = do
	threadDelay 3000000
	let refURL = if _refURL == "" then u else _refURL
	success <- rawSystem wgetCmd $ wgetParam refURL u prefix
	case success of
		GHC.IOBase.ExitSuccess -> wgetMain refURL prefix remains us
		GHC.IOBase.ExitFailure _ -> wgetMain refURL prefix (u:remains) us

getHrefURL :: String -> [String]
getHrefURL str = regMatch "href=\"[^\"]*\"" str

extractURL :: String -> String
extractURL ('h':'r':'e':'f':'=':'"':str) = reverse $ tail $ reverse str
extractURL _ = ""

isImageURL :: String -> Bool
isImageURL url = 
{-タブ-}	   case (map toLower $ takeExtension url) of
{-タブ-}{-タブ-}		".jpg" -> True
{-タブ-}{-タブ-}		".jpeg" -> True
{-タブ-}{-タブ-}		".gif" -> True
{-タブ-}{-タブ-}		".png" -> True
{-タブ-}{-タブ-}		_ -> False

isNotThumb :: String -> Bool
isNotThumb url = not $ (map toLower url) =~ "thumb" :: Bool

deleteDual :: [String] -> [String]
deleteDual strs = (map head) $ group strs

getFileName :: String -> String
getFileName url = head $ reverse $ splitOn "/" url

confURL :: String -> String
confURL ('t':'t':'p':url) = ('h':'t':'t':'p':url)
confURL url = url


-- from RWH Chapter 21
{- | Download a URL.  (Left errorMessage) if an error,
(Right doc) if success. -}
downloadURL :: String -> IO (Either String String)
downloadURL url =
    do resp <- simpleHTTP request
       case resp of
         Left x -> return $ Left ("Error connecting: " ++ show x)
         Right r -> 
             case rspCode r of
               (2,_,_) -> return $ Right (rspBody r)
               (3,_,_) -> -- A HTTP redirect
                 case findHeader HdrLocation r of
                   Nothing -> return $ Left (show r)
                   Just url -> downloadURL url
               _ -> return $ Left (show r)
    where request = Request {rqURI = uri,
                             rqMethod = GET,
                             rqHeaders = [],
                             rqBody = ""}
          uri = fromJust $ parseURI url

-- http://pikapika.to/~yf/m2/?Haskell を参考にした =~ ::[String] が使えなくなってたので
regMatch :: String -> String -> [String]
regMatch rx s =
   let (l, m, r) = (s =~ rx :: (String, String, String))
   in if null m then [] else m:regMatch rx r
