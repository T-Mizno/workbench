-- baseURLに相対パスrURLを連結して、パスを後ろから走査して、「..」があったら上に上っていく。

r2a :: String -> String -> String
r2a _ ('h':'t':'t':'p':':':'/':'/':rURL) = "http://" ++ rURL
r2a ('h':'t':'t':'p':':':'/':'/':baseURL) rURL = ("http://" ++ ) $ concatTer '/'  $ reverse $ r2a_rise2  (reverse $ (splitStr '/' (getBaseURL baseURL))) (reverse $(splitStr '/' rURL))
r2a _ _ = []

r2a_rise2 :: [String] -> [String] -> [String]
r2a_rise2 [] rrURL = rrURL
r2a_rise2 rBaseURL [] = rBaseURL
r2a_rise2 rBaseURL (".":rrURL) = r2a_rise2 rBaseURL rrURL
r2a_rise2 (b:rBaseURL) ("..":rrURL) = r2a_rise2_put rBaseURL rrURL
r2a_rise2 rBaseURL (r:rrURL) = r : r2a_rise2 rBaseURL rrURL

r2a_rise2_put :: [String] -> [String] -> [String]
r2a_rise2_put (b:rBaseURL) ("..":rURL) = r2a_rise2 rBaseURL rURL
r2a_rise2_put rBaseURL (".":rURL) = r2a_rise2 rBaseURL rURL
r2a_rise2_put rBaseURL (r:rURL) = r2a_rise2 (r:rBaseURL) rURL
r2a_rise2_put [] rURL = rURL
r2a_rise2_put rBaseURL [] = rBaseURL

-- 文字列のリストを連結する。ただし、文字列と文字列の間に文字terをはさむ。
concatTer :: Char -> [String] -> String
concatTer ter  = concat . (_concatTer ter)

_concatTer :: Char -> [String] -> [String]
_concatTer _ [] = []
_concatTer _ (x:[]) = [x]
_concatTer ter (x:xs) = x:(ter:[]): (_concatTer ter xs)

-- 文字列を文字terで分割する。 分割後の文字列にterは含めない。
splitStr :: Char -> String -> [String]
splitStr _ [] = []
splitStr ter str =
	let (pre, suf) = break (\x -> x == ter) str
	in pre : case suf of
		(ter:rest) -> splitStr ter rest
		_ -> []

lastStrIs :: Char -> String -> Bool
lastStrIs ter str =
	case reverse str of
		(_ter:_) -> _ter == ter
		_ -> False

getBaseURL :: String -> String
getBaseURL url =
	if lastStrIs '/' url
	then url
	else concatTer '/'  $ reverse $ tail $ (splitStr '/' ) $ reverse url
