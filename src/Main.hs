module Main where

import Control.Monad
import Data.List
import System.Process
import System.IO
import System.Directory
import System.FilePath               
import Network
import Data.Char

main::IO()
main =
 do { -- cabal "update" "Ampersand"
    ; -- clean
    --; mail
    ; notifyByMail "martijn@oblomov.com" "Testing" "One two three"
    --; r2 <- svnUpdate "Ampersand"
    --; print r2
    --; cabalCleanBuild "Ampersand"
    ; -- clean
    --; cabalCleanBuild "Prototype"
    --; cabalRun "Prototype" ["Test.adl", "--validate"]
    --; r <- getRevision "Ampersand"
    --; print r
    ; return ()
    }

cabalRun :: String -> [String] -> IO String
cabalRun project args =
 do { homeDir <- getHomeDirectory
    ; let executable = homeDir ++ "/svn/EclipseHaskell/" ++ project
                       ++ "/dist/build/" ++ project ++ "/" ++ project
    
    ; (resp, err) <- execute executable args $ homeDir ++ "/Dropbox/Oblomov/Projecten/Ampersand/ADL" 
    ; putStrLn resp
    ; if null err then return resp else error $ "running "++project++" failed: "++show (resp,err)
    }

cabalCleanBuild :: String -> IO ()
cabalCleanBuild project =
 do { cabal "clean" project
    ; cabal "configure" project
    ; cabal "build" project
    }

cabal :: String -> String -> IO ()
cabal cmd project =
 do { homeDir <- getHomeDirectory
    ; (resp, err) <- execute "cabal" [cmd] $ homeDir++"/svn/EclipseHaskell/"++project
    ; putStrLn resp
    ; if null err then return () else error $ "cabal "++cmd++" failed: "++show (resp,err)
    }

svnUpdate :: String -> IO ()
svnUpdate project =
 do { homeDir <- getHomeDirectory
    ; (resp, err) <- execute "svn" ["update","--non-interactive","--trust-server-cert"] $ homeDir++"/svn/EclipseHaskell/"++project
                                           -- parameters are because sourceforge sometimes changes the certificate which requires acceptation
    ; putStrLn resp
    ; if null err then return () else error $ "svn update failed: "++show (resp,err)
    }
 
getRevision :: String -> IO Int
getRevision project =
 do { homeDir <- getHomeDirectory
    ; (rev, err) <- execute "svnversion" [] $ homeDir++"/svn/EclipseHaskell/"++project
    ; case (rev,err) of
             ((_:_), "") | all isDigit $ init rev -> return $ read (init rev)
             _                                    -> error $ "incorrect response from svnversion: "++show (rev,err)
    }

-- call the command-line php with phpStr as input
execute :: String -> [String] -> String -> IO (String,String)
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
                 
    ; putStrLn $ "Execute: "++cmd++" "++intercalate " " args        
    ; (_, mStdOut, mStdErr, _) <- createProcess cp 
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
            --; putStrLn $ "Results:\n" ++ outputStr
            ; return (outputStr, errStr)
            }
    }
           
notifyByMail :: String -> String -> String -> IO ()
notifyByMail recipient subject message =
  sendMail "Ampersand Sentinel" "Stef.Joosten@ordina.nl" recipient ("[Sentinel] "++subject) message

sendMail :: String -> String -> String -> String -> String -> IO ()
sendMail sender senderName recipient subject body =
 do { let mailServer = "mail.kpnmail.nl"
    ; -- let mailServer = "smtp1.inter.NL.net"
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
                     ; if "Queued mail for delivery" `isInfixOf` message
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
{-
collectFilePaths base fileOrDir = 
  do { let path = combine base fileOrDir
     ; isDir <- doesDirectoryExist path
     ; if isDir then 
        do { fOrDs <- getProperDirectoryContents path
           ; fmap concat $ mapM (\fOrD -> readStaticFiles isBin base (combine fileOrDir fOrD)) fOrDs
           }
       else
        do { timeStamp@(TOD sec pico) <- getModificationTime path
           ; fileContents <- if isBin then fmap show $ BS.readFile path 
                                      else readFile path
           ; return ["SF "++show fileOrDir++" (TOD "++show sec++" "++show pico++"){- "++show timeStamp++" -} "++
                            show isBin++" "++show fileContents]
           }
     }
     
getProperDirectoryContents pth = fmap (filter (`notElem` [".","..",".svn"])) $ getDirectoryContents pth
-} 