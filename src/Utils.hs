module Utils 
  ( mkExecutionTest,isTestServer,getGitDir
  , bracketHtml,readMaybe
  , reportTestResult
  , getProperDirectoryContents
  , getAuthors, notifyByMail)
where

import Data.List
import System.Directory hiding (executable)
import System.FilePath
import System.IO
import Control.Monad               
import Network
import Network.BSD
import Types
import Defaults
import Data.Maybe

mkExecutionTest :: String -> IO ExecutionOutcome -> IO TestResult 
mkExecutionTest testDescr exec =
 do { execOutcome <- exec
    ; let testOutcome = case execOutcome of
                          ExecSuccess outp -> TestSuccess outp
                          ExecFailure _ err  -> TestFailure err -- exit code is already included in errMsg
    ; return $ TestResult testOutcome testDescr  
    }
    
reportTestResult :: Options -> IO TestResult -> IO TestResult
reportTestResult opts test =
 do { when (optHtml opts) $ putStrLn "<hr/>"
    ; testResult <- test
    ; case getResultOutcome testResult of
        TestSuccess _ -> putStrLn $ bracketHtml opts "<div style='color: green'>" "</div>"
                                      "Test passed"
        TestFailure _ -> putStrLn $ bracketHtml opts "<div style='color: red'>" "</div>" "Test did not pass." 
                                       -- ++ bracketHtml opts "<div style='font-family:courier; font-size:85%'>" "</div>" outp
                            -- todo: only show output for tests that should succeed
    ; return testResult -- return the result, so we can easily add this function to a computation
    } 

bracketHtml :: Options -> String -> String -> String -> String
bracketHtml opts open close str = if optHtml opts then open ++ str ++ close else str  

-- return True if we're running on the dedicated test server
isTestServer :: IO Bool
isTestServer = 
  fmap (== testServerHostname) getHostName
    
    
getGitDir :: IO FilePath
getGitDir =
 do { homeDir <- getHomeDirectory
    ; isTestSrv <- isTestServer  
    ; return $ homeDir </> gitPath isTestSrv 
    }

getProperDirectoryContents :: FilePath -> IO [String]
getProperDirectoryContents pth = fmap (filter (`notElem` [".",".."])) $ getDirectoryContents pth

readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of
                  [(a,"")] -> Just a
                  _        -> Nothing
getAuthors :: IO [String]
getAuthors =
 do { gitDir <- getGitDir
    ; let authorsFilePath = gitDir </> authorsFile
    ; authors <- readFile authorsFilePath
    ; return $ filter (not . null) . filter (not . ("--" `isPrefixOf`)) . lines $ authors
    }
        
notifyByMail :: [String] -> String -> String -> IO ()
notifyByMail recipients subject  =
  sendMail "Ampersand Sentinel" "noreply@sentinel.tarski.nl" recipients ("[Sentinel] "++subject) 

sendMail :: String -> String -> [String] -> String -> String -> IO ()
sendMail sender senderName recipients subject body =
 do { thisIstheTestserver <- isTestServer  
    ; let mailServer = if thisIstheTestserver 
                       then "mail.kpnmail.nl"
                       else "smtp1.inter.NL.net"
    ; putStrLn $ "connecting to " ++ mailServer
    
    ; handle <- connectTo mailServer (PortNumber 25)
    ; hSetNewlineMode handle $ NewlineMode LF CRLF -- NOTE: The output line mode needs to be CRLF for KPN
    ; let mailStr = mkMailStr sender senderName recipients subject body
    ; mapM_ (hPutStrLn handle) (lines mailStr)
--    ; hFlush handle
--    ; hPutStrLn handle "" -- no clue why this extra line+flush is necessary, but without it, sending mail hangs at
--    ; hFlush handle       -- Start mail input; end with <CRLF>.<CRLF>
--                          -- Might be buffer related, but changing Buffering mode does not help    
    ; (success,mmsg) <- processResponse handle
    ; putStrLn 
        (if success 
         then "message sent" 
         else "Sending mail failed: " ++ fromMaybe "(EoF)" mmsg)
    ; hClose handle
    }
 where processResponse handle = 
        do { eof <- hIsEOF handle
           ; if eof 
             then return (False,Nothing)
             else do { message <- hGetLine handle
                     ; putStrLn $ "SMTP server:" ++ message
                     ; if "Queued mail for delivery" `isInfixOf` message ||   -- KPN
                          "Message accepted for delivery" `isInfixOf` message -- InterNLnet
                       then return (True,Just message) 
                       else if "Timeout waiting for client input" `isInfixOf` message
                            then return (False,Just message)
                            else processResponse handle
                     }
           }

mkMailStr :: String -> String -> [String] -> String -> String -> String
mkMailStr senderName sender recipients subject body =
   unlines $
       [ "helo sentinel.tarski.nl"
       , "mail From: "++ sender
       ]
       ++ [ "rcpt To: "++recipient | recipient <- recipients ]
       ++ 
       [ "data"
       , "X-Mailer: Ampersand Sentinel"
       , "From: " ++ senderName ++ " <" ++ sender ++ ">"
       , "To: " ++ intercalate ", " recipients
       , "Subject: " ++ subject
       , ""
       ]  
       ++ filter (/= ".") (lines body) ++
       [ "." -- this is the trigger for smtp to end the mail body
       , "quit"
       , ""
       ]
 