module Utils where

import System.Directory hiding (executable)
import System.FilePath               
import Network.BSD
import Defaults

getSvnDir :: IO FilePath
getSvnDir =
 do { homeDir <- getHomeDirectory
    ; hName <- getHostName  
    ; return $ combine homeDir (if hName == testServerHostname
                                then testServerSvnPath
                                else oblomovSvnPath) 
    }
      
