module Execute where

import System.Process
import System.Exit
import System.IO
import System.FilePath               
import Data.Char
import Utils
import Types

testBuild :: String -> [String] -> IO TestResult
testBuild project flags = mkExecutionTest $
 do { putStrLn $ "Test: building "++project
    ; cabalConfigure project flags 
    ; result <- cabal "build" project []
    ; return $ result
    }


-- doing install without also building is not possible with cabal
testInstall :: String -> [String] -> IO TestResult
testInstall project flags = mkExecutionTest $
 do { putStrLn $ "Test: installing "++project
    ; cabalConfigure project flags 
    ; result <- cabal "install" project []
    ; return $ result
    }

cabalClean :: String -> [String] -> IO ()
cabalClean project flags = failOnError ("error during cabal clean for "++project++": ") $
  cabal "clean" project flags
  
cabalConfigure :: String -> [String] -> IO ()
cabalConfigure project flags = failOnError ("error during cabal configure for "++project++": ") $
  cabal "configure" project flags
   
cabal :: String -> String -> [String] -> IO ExecutionOutcome
cabal cmd project flags =
 do { svnDir <- getSvnDir
    ; result <- execute "cabal" (cmd : flags {- ++ ["--verbose"] -}) $ combine svnDir project
    ; return result -- when cabal fails silently, add --verbose
    }

svnUpdate :: String -> IO ()
svnUpdate project =
 do { svnDir <- getSvnDir
    ; result <- execute "svn" ["update","--non-interactive","--trust-server-cert"] $ combine svnDir project
                                         -- parameters are because sourceforge sometimes changes the certificate which requires acceptation
    ; case result of
        ExecSuccess _   -> return ()
        ExecFailure err -> error $ "error during svn update: " ++ err
    }

getRevisionStr :: String -> IO String
getRevisionStr project =
 do { svnDir <- getSvnDir
    ; result <- execute "svnversion" [] $ combine svnDir project
    ; case result of
             ExecSuccess rev@(_:_) -> return $ init rev -- only remove the newline
             ExecSuccess rev       -> error $ "incorrect response from svnversion: "++rev
             ExecFailure err       -> error $ "error from svnversion: "++err                                               
    }

getRevision :: String -> IO Int
getRevision project =
 do { revStr <- getRevisionStr project
    ; if all isDigit revStr
      then return $ read revStr
      else error $ "incorrect revision for "++project++": "++show revStr -- happens when a revision was locally modified
    }

-- call the command-line php with phpStr as input
execute :: String -> [String] -> String -> IO ExecutionOutcome
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
                 
--    ; putStrLn $ "Execute: "++cmd++" "++intercalate " " args ++ "   in "++dir      
    ; (_, mStdOut, mStdErr, pHandle) <- createProcess cp 
    ; case (mStdOut, mStdErr) of
        (Nothing, _) -> error "no output handle"
        (_, Nothing) -> error "no error handle"
        (Just stdOutH, Just stdErrH) ->
         do { --putStrLn "done"
            ; exitCode <- waitForProcess pHandle
            ; errStr <- hGetContents stdErrH
            ; seq (length errStr) $ return ()
            ; hClose stdErrH
            ; outputStr <- hGetContents stdOutH --and fetch the results from the output pipe
            ; seq (length outputStr) $ return ()
            ; hClose stdOutH
  --          ; putStrLn $ "Results:\n" ++ outputStr ++ "\nErrors\n:"++errStr++"\nDone<"
            ; return $ case exitCode of
                         ExitSuccess   -> ExecSuccess outputStr
                         ExitFailure c -> ExecFailure $ "Exit code " ++ show c ++ ": " ++ errStr
            }
    }

