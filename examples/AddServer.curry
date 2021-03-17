------------------------------------------------------------------------------
--- A simple "addition" server to test the library `Network.NamedSocket`.
---
--- @author Michael Hanus
--- @version March 2021
------------------------------------------------------------------------------

import System.IO
import Network.NamedSocket

-- The symbolic name for the server socket:
addSocketName :: String
addSocketName = "addserver"

sendTo :: String -> String -> IO ()
sendTo host msg = do
  h <- connectToSocket (addSocketName ++ "@" ++ host)
  hPutStr h msg
  hClose h

stopServer :: String -> IO ()
stopServer host = sendTo host "TERMINATE\n"


-- An "addition" server:
addServer :: IO ()
addServer = do
  socket <- listenOn addSocketName
  putStrLn $ "Serving socket: " ++ show addSocketName
  addServeSocket socket

addServeSocket :: Network.NamedSocket.Socket -> IO ()
addServeSocket socket = do
  (chost,stream) <- socketAccept socket
  putStrLn $ "Connection from " ++ chost
  serverLoop stream
 where
   serverLoop h = do
     l1 <- hGetLine h
     if l1=="TERMINATE"
      then do hClose h
              sClose socket
              putStrLn $ "Socket '" ++ socketName socket ++ "' closed"
      else do l2 <- hGetLine h
              hPutStrLn h $ show ((read l1 + read l2) :: Int)
              hClose h
              addServeSocket socket

addClient :: Show a => String -> a -> a -> IO ()
addClient host x y = do
  h <- connectToSocket (addSocketName ++ "@" ++ host)
  hPutStr h (unlines (map show [x,y]))
  hFlush h
  answer <- hGetLine h
  putStrLn $ "Answer: " ++ answer
  hClose h

{-
Test with PAKCS or KiCS2:

> :fork addServer
> addClient "localhost" 3 4
> stopServer "localhost"

-}
