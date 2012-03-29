module Execute where

import Data.List
import System.Process
import System.Exit
import System.IO
import System.FilePath               
import Network
import Network.BSD
import Data.Char
import Defaults
import Utils
import Types

testBuild :: String -> [String] -> IO TestResult
testBuild project flags =
 do { cabalConfigure project flags 
    ; result <- cabal "build" project []
    ; return $ result
    }


-- doing install without also building is not possible with cabal
testInstall :: String -> [String] -> IO TestResult
testInstall project flags =
 do { cabalConfigure project flags 
    ; result <- cabal "install" project []
    ; return $ result
    }

cabalClean :: String -> [String] -> IO ()
cabalClean project flags = failOnError ("error during cabal clean for "++project++": ") $
  --cabal "clean" project flags
  return $ Right "Clean is disabled"
  
cabalConfigure :: String -> [String] -> IO ()
cabalConfigure project flags = failOnError ("error during cabal configure for "++project++": ") $
  cabal "configure" project flags
   
cabal :: String -> String -> [String] -> IO TestResult
cabal cmd project flags =
 do { svnDir <- getSvnDir
    ; result <- execute "cabal" (cmd : flags) $ combine svnDir project
    ; return result
    }

svnUpdate :: String -> IO ()
svnUpdate project =
 do { svnDir <- getSvnDir
    ; result <- execute "svn" ["update","--non-interactive","--trust-server-cert"] $ combine svnDir project
                                         -- parameters are because sourceforge sometimes changes the certificate which requires acceptation
    ; case result of
        Right _ -> return ()
        Left err -> error $ "error during svn update: " ++ err
    }
 
getRevision :: String -> IO Int
getRevision project =
 do { svnDir <- getSvnDir
    ; result <- execute "svnversion" [] $ combine svnDir project
    ; case result of
             Right rev | all isDigit $ init rev -> return $ read (init rev)
                       | otherwise              -> error $ "incorrect response from svnversion: "++rev
             Left err                           -> error $ "error from svnversion: "++err                                               
    }

-- call the command-line php with phpStr as input
execute :: String -> [String] -> String -> IO TestResult
execute cmd args dir =
 do { let cp = CreateProcess
                { cmdspec      = RawCommand cmd args
                , cwd          = Just dir
                , env          = Nothing -- environment
                , std_in       = Inherit 
                , std_out      = CreatePipe
                , std_err      = CreatePipe
                , close_fds    = False -- no need to close all other file descriptors
                }
                 
    ; putStrLn $ "Execute: "++cmd++" "++intercalate " " args ++ "   in "++dir      
    ; (_, mStdOut, mStdErr, pHandle) <- createProcess cp 
    ; case (mStdOut, mStdErr) of
        (Nothing, _) -> error "no output handle"
        (_, Nothing) -> error "no error handle"
        (Just stdOutH, Just stdErrH) ->
         do { --putStrLn "done"
            ; errStr <- hGetContents stdErrH
            ; seq (length errStr) $ return ()
            ; hClose stdErrH
            ; outputStr <- hGetContents stdOutH --and fetch the results from the output pipe
            ; seq (length outputStr) $ return ()
            ; hClose stdOutH
            ; exitCode <- waitForProcess pHandle
            --; putStrLn $ "Results:\n" ++ outputStr
            ; return $ case exitCode of
                         ExitSuccess   -> Right outputStr
                         ExitFailure c -> Left $ "Exit code " ++ show c ++ ": " ++ errStr
            }
    }

notifyByMail :: String -> String -> String -> IO ()
notifyByMail recipient subject message =
  sendMail "Ampersand Sentinel" "Stef.Joosten@ordina.nl" recipient ("[Sentinel] "++subject) message

sendMail :: String -> String -> String -> String -> String -> IO ()
sendMail sender senderName recipient subject body =
 do { hName <- getHostName  
    ; let mailServer = if hName == testServerHostname 
                       then "mail.kpnmail.nl"
                       else "smtp1.inter.NL.net"
    ; putStrLn $ "connnecting to " ++ mailServer
    
    ; handle <- connectTo mailServer (PortNumber 25)
    ; hSetNewlineMode handle $ NewlineMode LF CRLF -- NOTE: The output line mode needs to be CRLF for KPN
    ; putStrLn "connected"

    ; let mailStr = mkMailStr sender senderName recipient subject body
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
                     --; putStrLn $ "SMTP server:" ++ message
                     ; if "Queued mail for delivery" `isInfixOf` message ||   -- KPN
                          "Message accepted for delivery" `isInfixOf` message -- InterNLnet
                       then return True 
                       else processResponse handle
                     }
           }

mkMailStr :: String -> String -> String -> String -> String -> String
mkMailStr senderName sender recipient subject body =
            "HELO amprersand.oblomov.com\n"
         ++ "MAIL From: "++ sender ++ "\n"
         ++ "RCPT To: "++ recipient ++ "\n"
         ++ "DATA\n"
         ++ "X-Mailer: Piglet 2.0\n"
         ++ "From: " ++ senderName ++ " <" ++ sender ++ ">\n"
         ++ "To: " ++ recipient ++ "\n"
         ++ "Subject: " ++ subject ++ "\n\n"
         ++ body ++ "\n"
         ++ ".\n"       
         ++ "QUIT\n"
