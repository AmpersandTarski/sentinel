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
    ; sendMessage
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

executeI :: String -> [String] -> String -> String -> IO (String,String)
executeI cmd args dir inp =
 do { let cp = CreateProcess
                { cmdspec      = RawCommand cmd args
                , cwd          = Just dir
                , env          = Nothing -- environment
                , std_in       = CreatePipe
                , std_out      = CreatePipe
                , std_err      = CreatePipe
                , close_fds    = False -- no need to close all other file descriptors
                }
                 
    ; putStrLn $ "Execute: "++cmd++" "++intercalate " " args        
    ; (mStdIn, mStdOut, mStdErr, pHandle) <- createProcess cp 
    ; case (mStdIn, mStdOut, mStdErr) of
        (Nothing, _, _) -> error "no input handle"
        (_, Nothing, _) -> error "no output handle"
        (_, _, Nothing) -> error "no error handle"
        (Just stdInH, Just stdOutH, Just stdErrH) ->
         do { putStrLn "1"
            ; hSetBuffering stdInH LineBuffering
    ; hSetNewlineMode stdInH $ NewlineMode LF LF
            ; hPutStrLn stdInH inp
            ; hFlush stdInH
            ; putStrLn "2"
           ; eof <- hIsEOF stdOutH
; hSetNewlineMode stdInH $ NewlineMode CRLF CRLF
            ; hPutStr stdInH "\n.\n"
               ; putStrLn $ "isEOF: "++show eof
          -- ; getResponse stdOutH
                     {-  ; errStr <- hGetContents stdErrH
            ; seq (length errStr) $ return ()
            ; hClose stdErrH -}
    
            ; putStrLn "3"
--            ;  outputStr <- hGetContents stdErrH --and fetch the results from the output pipe
--            ; print outputStr
            --; seq (length outputStr) $ return ()
            ; putStrLn "4" 
            ; mExitCode <- waitForProcess pHandle
            ; hClose stdInH
            ; hClose stdErrH
            ; hClose stdOutH
            ; print mExitCode
 --           ; hClose stdOutH
           
            --; putStrLn $ "Results:\n" ++ outputStr
            ; return ("outputStr", "")--errStr)
            }
    }

           
mail :: IO ()
mail  =
 do { --(resp, err) <- execute "echo" ["blaaa"] "" -- "Inp"
                                           -- parameters are because sourceforge sometimes changes the certificate which requires acceptation
    
    ; homeDir <- getHomeDirectory
    ; let mailStr = mailTxt "Ampersand Sentinel" "martijn@oblomov.com" "martijn@oblomov.com" "Tezzzt" "This is just a test!"
    --; putStrLn mailStr
    ; (resp, err) <- executeI "telnet" ["mail.kpnmail.nl","25"] homeDir mailStr
    ; 
    ; putStrLn resp
    ; if null err then return () else error $ "sending mail failed: "++show (resp,err)
    }

sendMessage =
  sendMail "Ampersand Sentinel" "martijn@oblomov.com" "martijn@oblomov.com" "Tezzzt" "This is just a test!"

sendMail :: String -> String -> String -> String -> String -> IO ()
sendMail sender senderName recipient subject body =
 do { let mailServer = "mail.kpnmail.nl"
    ; -- let mailServer = "smtp1.inter.NL.net"
    ; --let mailServer = "smtp.googlemail.com"
    ; putStrLn $ "connnecting to " ++ mailServer
    
    
    
    ; handle <- connectTo mailServer (PortNumber 25) 
    ; hSetNewlineMode handle $ NewlineMode LF CRLF
    --; hSetBuffering handle LineBuffering
    
--    ; hSetEncoding handle utf8
    ; str <- hShow handle
    ; putStrLn $ "handle: "++ str
    ; isw <- hIsWritable handle
    ; putStrLn $ "isWritable: "++show isw
    ; putStrLn "connected"
    ; putStr $ mailTxt sender senderName recipient subject body
    ; hPutStr handle $ mailTxt sender senderName recipient subject body
    ; hFlush handle
  --; hPutStrLn handle
    ; hFlush handle
    
    ; putStrLn "message sent"
    ; success <- printResponses handle
    ; print success
    }
 where printResponses handle = 
        do { eof <- hIsEOF handle
           ; putStrLn "2"
           ; putStrLn $ show eof
           ; if eof 
             then return False
             else do { message <- hGetLine handle
                     ; putStrLn $ "SMTP server:" ++ message
                     ; if "Queued mail for delivery" `isInfixOf` message
                       then return True 
                       else printResponses handle
                     }
           }
       getResponse h =
        do { l <- hGetLine h
           ; print l
           ; if "Queued mail" `isInfixOf` l then return () else getResponse h
           }
hShowGetLine h =
 do { l <- hGetLine h
    ; putStrLn l
    ; return l
    }

mailTxt senderName sender recipient subject body =
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