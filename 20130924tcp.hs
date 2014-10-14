import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import System.Exit

---
--- Server
---


type HandlerFunc = SockAddr -> String -> IO()

serveLog :: String -> HandlerFunc -> IO()
serveLog port handlerfunc = withSocketsDo $ do
    addrinfos <- withSocketsDo $ getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)  {- modify for avoid getAddrInfo error 10093 on Windows -}
    let serveraddr = head addrinfos

    sock <- socket (addrFamily serveraddr) Stream defaultProtocol

    bindSocket sock (addrAddress serveraddr)

    listen sock 5

    lock <- newMVar ()

    procRequests lock sock

  where
     procRequests :: MVar() -> Socket -> IO()
     procRequests lock mastersock =
       do
         (connsock, clientaddr) <- accept mastersock
         handle lock clientaddr "syslogtcpserver.hs: client connected"
         forkIO $ procMessage lock connsock clientaddr
         procRequests lock mastersock

     procMessage :: MVar() -> Socket -> SockAddr -> IO()
     procMessage lock connsock clientaddr = do
       connhdl <- socketToHandle connsock ReadMode
       hSetBuffering connhdl LineBuffering
       messages <- hGetContents connhdl
       mapM_ (handle lock clientaddr) (lines messages)
       hClose connhdl
       handle lock clientaddr "syslogtcpserver.hs: client disconnected"

     handle :: MVar() -> HandlerFunc
     handle lock clientaddr msg = withMVar lock (\a -> handlerfunc clientaddr msg >> return a)


plainHandler :: HandlerFunc
plainHandler _ "kill" = 
             exitWith ExitSuccess
plainHandler addr msg = 
  putStrLn $ "From " ++ show addr ++ ": " ++ msg


---
--- Client
---

data Priority =
   DEBUG
 | INFO
 | NOTICE
 | WARNING
 | ERROR
 | CRITICAL
 | ALERT
 | EMERGENCY
    deriving (Eq, Ord, Show, Read, Enum)

data Facility =
   KERN
 | USER
 | MAIL
 | DEAMON
 | AUTH
 | SYSLOG
 | LPR
 | NEWS
 | UUCP
 | CRON
 | AUTHPRIV
 | FTP
 | LOCAL0
 | LOCAL1
 | LOCAL2
 | LOCAL3
 | LOCAL4
 | LOCAL5
 | LOCAL6
 | LOCAL7
    deriving (Eq, Show, Read)

facToCode = [
   (KERN, 0)
 , (USER, 1)
 , (MAIL, 2)
 , (DEAMON, 3)
 , (AUTH, 4)
 , (SYSLOG, 5)
 , (LPR, 6)
 , (NEWS, 7)
 , (UUCP, 8)
 , (CRON, 9)
 , (AUTHPRIV, 10)
 , (FTP, 11)
 , (LOCAL0, 16)
 , (LOCAL1, 17)
 , (LOCAL2, 18)
 , (LOCAL3, 19)
 , (LOCAL4, 20)
 , (LOCAL5, 21)
 , (LOCAL6, 22)
 , (LOCAL7, 23)
 ]

codeToFac = map (\(x, y) -> (y, x)) facToCode

codeOfFac :: Facility -> Int
codeOfFac f = case lookup f facToCode of
   Just x -> x
   _ -> error $ "Internal error in codeOfFac"

facOfCode :: Int -> Facility
facOfCode f = case lookup f codeToFac of
   Just x -> x
   _ -> error $ "Invalid code in facOfCode"

data SyslogHandle = SyslogHandle {slHandle :: Handle, slProgram :: String}

openlog :: HostName -> String -> String -> IO SyslogHandle
openlog hostname port progname = do
   addrinfos <- withSocketsDo $ getAddrInfo Nothing (Just hostname) (Just port)  {- add for getAddrInfo error 10093 on Windows -}
   let serveraddr = head addrinfos

   sock <- socket (addrFamily serveraddr) Stream defaultProtocol

   setSocketOption sock KeepAlive 1

   connect sock (addrAddress serveraddr)

   h <- socketToHandle sock WriteMode

   hSetBuffering h (BlockBuffering Nothing)

   return $ SyslogHandle h progname


syslog :: SyslogHandle -> Facility -> Priority -> String -> IO()
syslog syslogh fac pri msg = do
    hPutStrLn (slHandle syslogh) sendmsg
    hFlush (slHandle syslogh)
  where
     code = makeCode fac pri
     sendmsg = "<" ++ show code ++ ">" ++ (slProgram syslogh) ++ ": " ++ msg

closelog :: SyslogHandle -> IO()
closelog syslogh = hClose (slHandle syslogh)

makeCode :: Facility -> Priority -> Int
makeCode fac pri =
  let
     faccode = codeOfFac fac
     pricode = fromEnum pri
  in
     (faccode `shiftL` 3) .|. pricode
