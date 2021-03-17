------------------------------------------------------------------------------
--- Configurations of the implementation of a Curry Port Name Server (CPNS)
--- based on raw sockets. The CPNS allows the connection to port by
--- symbolic names.
---
--- If we connect to a port with symbolic name `pn`, we first connect
--- to the CPNS of the host named by `pn` to get the physical socket
--- number of this port. In order to connect to CPNS from any
--- machine in the world, the CPNS demon always listens at port `cpnsSocket`.
--- Note that this port must be identical for all machines running
--- `Network.NamedSocket` or Distributed Curry! If this port is occupied
--- by another process on a host, you cannot run CPNSD on it.
---
--- @author Michael Hanus
--- @version March 2021
------------------------------------------------------------------------------

module Network.CPNS.Config
  ( cpnsSocket, cpnsTimeOut
  , getCPNSD, getStartupLockFile, getLogFile, addLogLn
  )
 where

import System.Directory   ( doesFileExist, getTemporaryDirectory )
import System.FilePath    ( (</>) )

import Network.CPNS.ConfigPackage ( packageExecutable )


-- The standard port number of CPNS demon.
cpnsSocket :: Int
cpnsSocket = 8769

-- The time out before considering the server as unreachable:
cpnsTimeOut :: Int
cpnsTimeOut = 6000

--- Gets name of lock file to coordinate the startup of the CPNS demon.
getStartupLockFile :: IO String
getStartupLockFile = do
  tmp <- getTemporaryDirectory
  return $ tmp </> "Curry_CPNSD.lock"

--- Returns the executable of the CPNS demon.
--- Raises an error if the executable does not exist.
getCPNSD :: IO String
getCPNSD = do
  let cpnsbin = packageExecutable
  exbin <- doesFileExist cpnsbin
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
