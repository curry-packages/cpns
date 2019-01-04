------------------------------------------------------------------------------
--- Implementation of a Curry Port Name Server based on raw sockets.
--- It is used to implement the library `Network.NamedPorts` and
--- the library `Ports` for distributed programming with ports.
---
--- @author Michael Hanus
--- @version January 2019
------------------------------------------------------------------------------

module Network.CPNS
  ( registerPort, getPortInfo, unregisterPort
  , cpnsStart, cpnsStop, cpnsStatus, cpnsAlive
  )
 where

import Char     ( isSpace )
import IO
import List     ( delete )
import System
import Time

import Debug.Profile  ( getProcessInfos, showMemInfo )
import Network.Socket

import Network.CPNS.Config

--- Type of messages to be processed by the Curry Port Name Server.
---
--- @cons Register name pid sn pn
---       -  assign the values pid, sn, and pn to name
---          (pid is the process number of the registered process
---           (should be 0 if it is unknown); the server returns True
---           if registration had no problems, otherwise False)
--- @cons GetRegister name - request for a registered port name;
---         the server returns the values (sn,pn) that are assigned to the
---         port name
--- @cons Unregister name - request to remove a registered port name
--- @cons ShowRegistry - show status and current port registrations
--- @cons Ping         - ping the CPNS demon for liveness check
--- @cons Terminate    - terminate the CPNS demon
data CPNSMessage = Terminate
                 | Ping
                 | Register String Int Int Int
                 | GetRegister String
                 | Unregister String
                 | ShowRegistry
  deriving (Read, Show)

--- Starts the "Curry Port Name Server" (CPNS) running on the local machine.
--- The CPNS is responsible to resolve symbolic names for ports
--- into physical socket numbers so that a port can be reached
--- under its symbolic name from any machine in the world.

cpnsStart :: IO ()
cpnsStart = catch startup
                  (\_ -> addLogLn "FAILURE occurred during startup!" >>
                         deleteStartupLockfile >>
                         return Nothing) >>=
             maybe done (cpnsServer [])
 where
   deleteStartupLockfile = do
     lockfile <- getStartupLockFile
     addLogLn $ "Removing startup lock file \"" ++ lockfile ++ "\"..."
     system $ "/bin/rm -f " ++ lockfile
     done

   startup = do
     addLogLn $ "Starting Curry Port Name Server on port " ++
                show cpnsSocket ++ "..."
     socket <- listenOn cpnsSocket
     deleteStartupLockfile
     pid <- getPID
     addLogLn $ "Curry Port Name Server is ready (PID: "++show pid++")."
     return (Just socket)

--- The main loop of the CPNS demon
cpnsServer :: [(String,Int,Int,Int)] -> Socket -> IO ()
cpnsServer regs socket = do
  (chost,stream) <- accept socket
  --addLogLn $ "Connection from "++chost
  serveRequest chost stream
 where
   doIfLocalHost rhost action = do
     hostname <- getHostname
     if rhost `elem` ["localhost","localhost.localdomain",hostname]
        || take 8 rhost == "127.0.0."
      then action
      else do addLogLn $ "Illegal request received from host: " ++ rhost
              cpnsServer regs socket

   serveRequest rhost h = do
     msg <- readMsgLine h
     either
      (\line -> do addLogLn $ "ERROR: Illegal message:\n" ++ line
                   cpnsServer regs socket )
      (\m -> case m of
        Terminate -> doIfLocalHost rhost $ do
          hClose h
          addLogLn "CPNS demon terminated."
        Ping -> do
          hPutStrLn h (show True)
          hClose h
          cpnsServer regs socket
        Register pname pid sn pn -> doIfLocalHost rhost $ do
          (ack, newregs) <- tryRegisterPortName regs pname pid sn pn
          hPutStrLn h (show ack)
          hClose h
          cpnsServer (id $!! newregs) socket
        GetRegister pname -> do
          --addLogLn $ "Request for port name: " ++ pname
          (newregs,pinfo) <- getRegisteredPortName regs pname
          hPutStrLn h (show pinfo)
          hClose h
          cpnsServer (id $!! newregs) socket
        Unregister pname -> doIfLocalHost rhost $ do
          newregs <- unregisterPortName regs pname
          hClose h
          cpnsServer (id $!! newregs) socket
        ShowRegistry -> doIfLocalHost rhost $ do
          newregs <- getAndCleanRegs regs
          meminfo <- getMemInfo regs
          pid     <- getPID
          hPutStrLn h (show (pid,meminfo,newregs))
          hClose h
          cpnsServer (id $!! newregs) socket )
      msg

tryRegisterPortName :: [(String,Int,Int,Int)] -> String -> Int -> Int -> Int
                    -> IO (Bool, [(String, Int, Int, Int)])
tryRegisterPortName regs name pid sn pn = do
  let nameregs = filter (\(n,_,_,_) -> name==n) regs
  ack <- if null nameregs
         then return True
         else let (_,pid',_,_) = head nameregs in
              if pid'>0 && pid'/=pid
              -- we allow registration from the same process
              then doesProcessExist pid' >>= \pex -> return (not pex)
              else return True
  ctime <- getLocalTime
  addLogLn $ "Register port \""++name++"\": pid "++show pid++
             " / socket "++show sn++
             " / number "++show pn ++ " at " ++ calendarTimeToString ctime
  let newregs = (name,pid,sn,pn) : filter (\ (n,_,_,_) -> name/=n) regs
  getMemInfo newregs >>= addLogLn
  hFlush stdout
  return (ack, newregs)

-- Delete all registrations for a given port name:
unregisterPortName :: [(String,Int,Int,Int)] -> String
                   -> IO [(String,Int,Int,Int)]
unregisterPortName regs name = do
  ctime <- getLocalTime
  addLogLn $ "Unregister port \""++name++"\" at "++calendarTimeToString ctime
  let newregs = filter (\ (n,_,_,_) -> name/=n) regs
  getMemInfo newregs >>= addLogLn
  hFlush stdout
  return newregs

-- Get the socket number for a registered port name
-- (or (0,0) if not registered).
-- In addition, a new registration list is returned where a registration
-- is deleted if the corresponding server process does not exist.
getRegisteredPortName :: [(String,Int,Int,Int)] -> String
                      -> IO ([(String,Int,Int,Int)],(Int,Int))
getRegisteredPortName regs pname =
  let nameregs = filter (\(n,_,_,_)->pname==n) regs in
  if null nameregs
  then return (regs,(0,0))
  else let (_,pid,sn,pn) = head nameregs in
       if pid>0
       then doesProcessExist pid >>= \pex ->
            if pex
            then return (regs,(sn,pn))
            else --addLogLn ("WARNING: Process "++show pid++" not running!") >>
                 return (delete (head nameregs) regs, (0,0))
       else return (regs,(sn,pn))

-- Returns the registration list but delete a registration
-- if the corresponding server process does not exist.
getAndCleanRegs :: [(String,Int,Int,Int)] -> IO [(String,Int,Int,Int)]
getAndCleanRegs regs = do
  newreglist <- mapIO checkAndShow regs
  return (concat newreglist)
 where
  checkAndShow reg@(_,pid,_,_) = do
    pidexist <- doesProcessExist pid
    return $ if pidexist then [reg] else []

-- Returns memory status information of current CPNS demon process as string.
getMemInfo :: [(String,Int,Int,Int)] -> IO String
getMemInfo regs = do
  pinfos <- getProcessInfos
  return $ "NumRegs: " ++ show (length regs) ++ ", " ++ showMemInfo pinfos

-- Tests whether a process with a given pid is running.
doesProcessExist :: Int -> IO Bool
doesProcessExist pid = do
  status <- system("test -z \"`ps -p "++show pid++" | fgrep "++show pid++"`\"")
  return (status>0)

-- Read a line from a stream and check syntactical correctness of message:
readMsgLine :: Read a => Handle -> IO (Either String a)
readMsgLine handle = do
  line <- hGetLine handle
  case reads line of
     [(msg,rem)] -> return (if all isSpace rem then Right msg else Left line)
     _ -> return (Left line)

-- Perform an action if the CPNS demon at a given host is alive:
doIfAlive :: String -> IO a -> IO a
doIfAlive host action = do
  alive <- cpnsAlive host
  if not alive
   then error $ "Curry port name server at host \"" ++ host ++
                "\" is not reachable (timeout after " ++ show cpnsTimeOut ++
                " ms)!"
   else action

sendToLocalCPNS :: CPNSMessage -> IO ()
sendToLocalCPNS msg = doIfAlive "localhost" $ do
  h <- connectToSocket "localhost" cpnsSocket
  hPutStrLn h (show msg)
  hClose h

--- Shows all registered ports at the local CPNS demon (in its logfile).
cpnsStatus :: IO ()
cpnsStatus = do
  (pid,meminfo,regs) <- cpnsTryGetAnswer "localhost" ShowRegistry
  putStrLn $ "CPNSD PID: " ++ show (pid :: Int)
  putStrLn $ "Memory information:\n" ++ meminfo
  putStrLn "Currently registered port names:"
  putStrLn $ unlines $ map showReg regs

showReg :: (String,Int,Int,Int) -> String
showReg (n,pid,sn,pn) = n ++ ": pid " ++ show pid ++
                        " / socket " ++ show sn ++ " / number " ++ show pn

--- Terminates the local CPNS demon
cpnsStop :: IO ()
cpnsStop = sendToLocalCPNS Terminate

--- Gets an answer from a Curry port name server on a host,
--- or reports an error.
cpnsTryGetAnswer :: Read a => String -> CPNSMessage -> IO a
cpnsTryGetAnswer host msg = catch tryGetAnswer connectError
 where
  tryGetAnswer = do
    h <- connectToSocket host cpnsSocket
    hPutStrLn h (show msg)
    hFlush h
    ready <- hWaitForInput h cpnsTimeOut
    if ready
     then do
       answer <- readMsgLine h
       hClose h
       either (\line -> error $ "cpnsTryGetAnswer: Illegal answer: " ++ line)
              return
              answer
     else failed

  connectError _ = error $ "Curry port name server at host \""++host++
                           "\" is not reachable!"

--- Registers a symbolic port at the local host.
--- The symbolic name, the socket number, and the port number are passed
--- as arguments.
registerPort :: String -> Int -> Int -> IO ()
registerPort pname sn pn = do
  startCPNSDIfNecessary
  pid <- getPID
  ack <- cpnsTryGetAnswer "localhost" (Register pname pid sn pn)
  if ack then done
         else addLogLn ("WARNING: Port name '"++pname++"' already registered!")

--- Gets the information about a symbolic port (first argument)
--- at some host (second argument).
--- If there is no registration, `(0,0)` is returned, otherwise a pair
--- consisting of a socket and port number.
getPortInfo :: String -> String -> IO (Int,Int)
getPortInfo pname host = cpnsTryGetAnswer host (GetRegister pname)

--- Unregisters a symbolic port at the local host.
unregisterPort :: String -> IO ()
unregisterPort pname = sendToLocalCPNS (Unregister pname)

--- Tests whether the CPNS demon at a host is alive, i.e.,
--- reacts on `Ping` message.
cpnsAlive :: String -> IO Bool
cpnsAlive host = catch tryPingCPNS (\_ -> return False)
 where
  tryPingCPNS = do
    h <- connectToSocket host cpnsSocket
    hPutStrLn h (show Ping)
    hFlush h
    answer <- hWaitForInput h cpnsTimeOut
    hClose h
    return answer

--- Starts the CPNS demon at localhost if it is not already running:
startCPNSDIfNecessary :: IO ()
startCPNSDIfNecessary = do
  alive <- cpnsAlive "localhost"
  unless alive $ do
    cpnsbin <- getCPNSD
    system $ cpnsbin ++ " start"
    done

{-
Testing:

Network.CPNS> registerPort "xxx" 42 2
Network.CPNS> getPortInfo "xxx" "localhost"
(42,2)

-}
