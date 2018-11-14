------------------------------------------------------------------------
--- This is the main program of the CPNS demon.
---
--- @author Michael Hanus
--- @version November 2018
------------------------------------------------------------------------

module Network.CPNSD
 where

import Directory ( doesFileExist )
import IO        ( hFlush, hPutStrLn, stderr )
import System    ( getArgs, getPID, system )
import Time      ( calendarTimeToString, getLocalTime )

import Network.CPNS
import Network.CPNS.Config

--- Main function for CPNS demon. Check arguments and execute command.
main :: IO ()
main = do
  args <- getArgs
  case args of
   ["serve"]   -> cpnsStart
   ["start"]   -> startServerIfNecessary
   ["stop"]    -> stopServer
   ["status"]  -> cpnsStatus
   ["log"]     -> do logfile <- getLogFile
                     readFile logfile >>= putStr
   _ -> putErrLn $ "ERROR: Illegal arguments: " ++ unwords args ++ "\n" ++
                    "Allowed arguments: start | stop | serve | status | log"

--- Start the Curry Port Name Server demon, if it is not already
--- started on this machine:
startServerIfNecessary :: IO ()
startServerIfNecessary = do
  alive <- cpnsAlive "localhost"
  unless alive $ do
    putErrLn "CPNS demon seems to be aborted. I try to restart it..."
    startServer

startServer :: IO ()
startServer = do
  cpnsbin <- getCPNSD
  lockfile <- getStartupLockFile
  lfc <- system $ "lockfile-create --lock-name " ++ lockfile
  if lfc == 0
    then do
      putErrLn "Starting demon for Curry Port Name Server..."
      logfile <- getLogFile
      exlogfile <- doesFileExist logfile
      unless exlogfile $ do
        -- create log file with correct access rights:
        system $ "touch " ++ logfile
        -- make log file accessible for everybody:
        system $ "chmod -f 666 " ++ logfile
        done
      putErrLn $ "Log information in file '" ++ logfile ++ "'"
      ctime <- getLocalTime
      appendFile logfile $
        "CPNS demon started at " ++ calendarTimeToString ctime ++ "\n"
      -- start CPNSD in server mode:
      system $ "nohup \"" ++ cpnsbin ++ "\" serve >> " ++ logfile ++ " 2>&1  &"
      lockFileCreateAndRemove lockfile
      putErrLn "CPNS demon started."
    else do
      putErrLn "CPNS demon seems already started by another process"
      putErrLn $ "If this is not the case, delete file '" ++ lockfile ++ "'"
      lockFileCreateAndRemove lockfile
 where
  -- wait for lockfile deletion by CPNS demon startup:
  lockFileCreateAndRemove lockfile = do
    system $ "lockfile-create --lock-name " ++ lockfile
    system $ "lockfile-remove --lock-name " ++ lockfile
    done

--- Terminate the Curry Port Name Server demon, if it is not already terminated.
stopServer :: IO ()
stopServer = do
  lockfile <- getStartupLockFile
  system $ "/bin/rm -f " ++ lockfile -- just to be sure
  -- test whether the server process is still alive:
  alive <- cpnsAlive "localhost"
  if alive then cpnsStop
           else putErrLn "CPNS demon does not seem to be alive"

putErrLn :: String -> IO ()
putErrLn s = hPutStrLn stderr s >> hFlush stderr
