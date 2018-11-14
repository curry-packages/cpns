------------------------------------------------------------------------------
--- Configurations of the implementation of a Curry Port Name Server
--- based on raw sockets.
---
--- @author Michael Hanus
--- @version November 2018
------------------------------------------------------------------------------

module Network.CPNS.Config
  ( cpnsSocket, cpnsTimeOut
  , getCPNSD, getStartupLockFile, getLogFile, addLogLn
  )
 where

import Directory    ( doesFileExist, getTemporaryDirectory )
import FilePath     ( (</>) )
import List         ( splitOn )
import System       ( getEnviron )

import System.Path  ( fileInPath )

-- If we connect to a port with symbolic name pn, we first connect
-- to the CPNS of the host named by pn to get the physical socket
-- number of this port. In order to connect to CPNS from any
-- machine in the world, the CPNS demon always listens at port `cpnsSocket`.
-- (Note that this must be identical for all machines running
-- `Network.NamedSocket` or Distributed Curry! If this port is occupied
-- by another process on a host, you cannot run CPNSD on it.)

-- The standard port number of CPNS demon.
cpnsSocket :: Int
cpnsSocket = 8769


-- The time out before considering the server as unreachable:
cpnsTimeOut :: Int
cpnsTimeOut = 3000

--- Gets name of lock file to coordinate the startup of the CPNS demon.
getStartupLockFile :: IO String
getStartupLockFile = do
  tmp <- getTemporaryDirectory
  return $ tmp </> "Curry_CPNSD.lock"

--- Returns the executable of the CPNS demon.
--- Raises an error if the executable does not exist.
getCPNSD :: IO String
getCPNSD = do
  let cpnsbin = "curry-cpnsd"
  exbin <- fileInPath cpnsbin
  if exbin
    then return cpnsbin
    else error "CPNS demon not found in path! Install it by: cypm install cpns"

--- Get name of CPNSD log file.
getLogFile :: IO String
getLogFile = do
  tmp <- getTemporaryDirectory
  return $ tmp </> "Curry_CPNSD.log"

--- Adds a line to the log file
addLogLn :: String -> IO ()
addLogLn s = do
  logfile <- getLogFile
  appendFile logfile (s ++ "\n")
