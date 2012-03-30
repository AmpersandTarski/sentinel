module Utils where

import Data.List
import System.Directory hiding (executable)
import System.FilePath
import System.IO               
import Network
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
getAuthors :: IO [String]
getAuthors =
 do { svnDir <- getSvnDir
    ; let authorsFilePath = combine svnDir authorsFile
    ; authors <- readFile authorsFilePath
    ; return $ filter (not . null) . filter (not . ("--" `isPrefixOf`)) . lines $ authors
    }
        
notifyByMail :: [String] -> String -> String -> IO ()
notifyByMail recipients subject message =
  sendMail "Ampersand Sentinel" "Stef.Joosten@ordina.nl" recipients ("[Sentinel] "++subject) message

sendMail :: String -> String -> [String] -> String -> String -> IO ()
sendMail sender senderName recipients subject body =
 do { hName <- getHostName  
    ; let mailServer = if hName == testServerHostname 
                       then "mail.kpnmail.nl"
                       else "smtp1.inter.NL.net"
    ; putStrLn $ "connnecting to " ++ mailServer
    
    ; handle <- connectTo mailServer (PortNumber 25)
    ; hSetNewlineMode handle $ NewlineMode LF CRLF -- NOTE: The output line mode needs to be CRLF for KPN
    ; putStrLn "connected"

    ; let mailStr = mkMailStr sender senderName recipients subject body
    --; putStrLn $ "Output to mail server:\n" ++ mailStr
    ; hPutStr handle mailStr
    ; hFlush handle
    ; hPutStrLn handle "" -- no clue why this extra line+flush is necessary, but without it, sending mail hangs at
    ; hFlush handle       -- Start mail input; end with <CRLF>.<CRLF>
                          -- Might be buffer related, but changing Buffering mode does not help    

    ; success <- processResponse handle
    ; if success then putStrLn "message sent" else error "Sending mail failed"
    }
 where processResponse handle = 
        do { eof <- hIsEOF handle
           ; if eof 
             then return False
             else do { message <- hGetLine handle
--                     ; putStrLn $ "SMTP server:" ++ message
                     ; if "Queued mail for delivery" `isInfixOf` message ||   -- KPN
                          "Message accepted for delivery" `isInfixOf` message -- InterNLnet
                       then return True 
                       else processResponse handle
                     }
           }

mkMailStr :: String -> String -> [String] -> String -> String -> String
mkMailStr senderName sender recipients subject body =
            "HELO amprersand.oblomov.com\n"
         ++ "MAIL From: "++ sender ++ "\n"
         ++ unlines [ "RCPT To: "++recipient | recipient <- recipients ]
         ++ "DATA\n"
         ++ "X-Mailer: Ampersand Sentinel\n"
         ++ "From: " ++ senderName ++ " <" ++ sender ++ ">\n"
         ++ "To: " ++ intercalate ", " recipients ++ "\n"
         ++ "Subject: " ++ subject ++ "\n\n"
         ++ body ++ "\n"
         ++ ".\n"       
         ++ "QUIT\n"
 