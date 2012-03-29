module Utils where

import System.Directory hiding (executable)
import System.FilePath               
import Network.BSD
import Types
import Defaults
    
failOnError :: String -> IO TestResult -> IO ()
failOnError errMsg test =
 do { result <- test
    ; case result of
        Right _  -> return ()
        Left err -> error $ errMsg ++ err
    }
    
reportResult :: IO TestResult -> IO ()
reportResult test =
 do { result <- test
    ; case result of
        Right outp -> putStrLn $ "Success: " {- ++ outp -} ++ "\n\n\n" 
        Left err ->   putStrLn $ "Failure: "++err++"\n\n\n" 
    } 
    
getSvnDir :: IO FilePath
getSvnDir =
 do { homeDir <- getHomeDirectory
    ; hName <- getHostName  
    ; return $ combine homeDir (if hName == testServerHostname
                                then testServerSvnPath
                                else oblomovSvnPath) 
    }

getProperDirectoryContents :: FilePath -> IO [String]
getProperDirectoryContents pth = fmap (filter (`notElem` [".","..",".svn"])) $ getDirectoryContents pth
    