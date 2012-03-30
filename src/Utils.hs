module Utils where

import System.Directory hiding (executable)
import System.FilePath               
import Network.BSD
import Types
import Defaults
    
failOnError :: String -> IO ExecutionOutcome -> IO ()
failOnError errMsg test =
 do { result <- test
    ; case result of
        ExecSuccess _  -> return ()
        ExecFailure err -> error $ errMsg ++ err
    }
 
mkExecutionTest :: IO ExecutionOutcome -> IO TestResult 
mkExecutionTest exec =
 do { execOutcome <- exec
    ; let testOutcome = case execOutcome of
                          ExecSuccess outp -> TestSuccess outp
                          ExecFailure err  -> TestFailure err
    ; return $ TestResult testOutcome $ TestSpec Ampersand ["todo: this is wrong"] ShouldSucceed []  
    }
    
reportTestResult :: IO TestResult -> IO TestResult
reportTestResult test =
 do { testResult <- test
    ; case getResultOutcome testResult of
        TestSuccess _ -> putStrLn $ "Test succeeded\n"
        TestFailure _ -> putStrLn $ "Test failed\n"
    ; return testResult -- return the result, so we can easily add this function to a computation
    } 
    
-- return True if we're running on the dedicated test server
isTestServer :: IO Bool
isTestServer = 
 do { fmap (== testServerHostname) getHostName
    }
    
getSvnDir :: IO FilePath
getSvnDir =
 do { homeDir <- getHomeDirectory
    ; isTestSrv <- isTestServer  
    ; return $ combine homeDir (if isTestSrv
                                then testServerSvnPath
                                else oblomovSvnPath) 
    }

getProperDirectoryContents :: FilePath -> IO [String]
getProperDirectoryContents pth = fmap (filter (`notElem` [".","..",".svn"])) $ getDirectoryContents pth

readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of
                  [(a,"")] -> Just a
                  _        -> Nothing
    