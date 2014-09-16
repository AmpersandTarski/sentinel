module Execute where

import System.Process
import System.Timeout
import System.Exit
import System.IO
import System.FilePath 
import Control.Exception
import Data.List              
import Data.Char
import Utils
import Types

maxTimeInSeconds :: Int
maxTimeInSeconds = 10 * 60

timeoutExitCode :: Int
timeoutExitCode = -1 -- -1 is not used by unix processes



testBuild :: String -> [String] -> String -> IO TestResult
testBuild project flags targetDescr = mkExecutionTest testDescr $
 do { putStrLn testDescr
    ; cabalConfigure project flags 
    ; executeCabal True "build" project []
    }
 where testDescr = "Building "++targetDescr++". (project: "++project++", flags: ["++intercalate ", " flags++"]) {should succeed}"

-- doing install without also building is not possible with cabal. UPDATE: maybe no longer true with recent cabal versions
testInstall :: String -> [String] -> String -> IO TestResult
testInstall project flags targetDescr = mkExecutionTest testDescr $
 do { putStrLn testDescr
    -- cabal install may take a long time because of the dependencies, so we disable the time limit (it will always finish)
    ; executeCabal False "install" project flags -- pass flags directly to install (cabal install ignores cabal configure)
    }
 where testDescr = "Building and installing "++targetDescr++". (project: "++project++", flags: ["++intercalate ", " flags++"]) {should succeed}"

cabalUpdate :: IO ()
cabalUpdate = failOnError "error during cabal update" $
  executeCabal True "update" "." [] -- just pass . as project dir

cabalDeleteSandbox :: String -> IO ()
cabalDeleteSandbox project = -- cannot fail on error, as delete fails if there is no sandbox
 do { putStrLn $ "Deleting "++project++" sandbox, may show a harmless error message if no sandbox was present."
    ; result <- executeCabal True "sandbox" project ["delete"]
    ; case result of
        ExecSuccess _     -> return ()
        ExecFailure _ err -> putStr $ err
    }
  
cabalInitSandbox :: String -> [String] -> IO ()
cabalInitSandbox project flags = failOnError ("error during \'cabal sandbox init\' for "++project++": ") $
  executeCabal True "sandbox" project ("init" : flags) 
 
cabalClean :: String -> [String] -> IO ()
cabalClean project flags = cabalCmd "clean" project flags
  
cabalConfigure :: String -> [String] -> IO ()
cabalConfigure project flags = cabalCmd "configure" project flags

cabalCopy :: String -> [String] -> IO ()
cabalCopy project flags = cabalCmd "copy" project flags

cabalRegister :: String -> [String] -> IO ()
cabalRegister project flags = cabalCmd "register" project flags

cabalCmd :: String -> String -> [String] -> IO ()
cabalCmd cmd project flags = failOnError ("error during \'cabal "++cmd++"\' for "++project++": ") $
  executeCabal True cmd project flags

executeCabal :: Bool -> String -> String -> [String] -> IO ExecutionOutcome
executeCabal hasTimeLimit cmd project flags =
 do { svnDir <- getSvnDir
    ; execute hasTimeLimit "cabal" (cmd : flags ) $ combine svnDir project
    }

svnUpdate :: String -> IO ()
svnUpdate project =
 do { svnDir <- getSvnDir
    ; result <- execute True "svn" ["update","--non-interactive","--trust-server-cert"] $ combine svnDir project
                                         -- parameters are because sourceforge sometimes changes the certificate which requires acceptation
    ; case result of
        ExecSuccess _     -> do { revStr <- getRevisionStr project
                                ; putStrLn $ project ++ " revision: " ++ revStr 
                                }
        ExecFailure _ err -> error $ "error during svn update: " ++ err -- exit code is already included in errMsg
    }

getRevisionStr :: String -> IO String
getRevisionStr project =
 do { svnDir <- getSvnDir
    ; result <- execute True "svnversion" [] $ combine svnDir project
    ; case result of
             ExecSuccess rev@(_:_) -> return $ init rev -- only remove the newline
             ExecSuccess rev       -> error $ "incorrect response from svnversion: "++rev
             ExecFailure _ err     -> error $ "error from svnversion: "++err -- exit code is already included in errMsg                                               
    }

getRevision :: String -> IO Int
getRevision project =
 do { revStr <- getRevisionStr project
    ; if all isDigit revStr
      then return $ read revStr
      else error $ "incorrect revision for "++project++": "++show revStr -- happens when a revision was locally modified
    }

execute :: Bool -> String -> [String] -> String -> IO ExecutionOutcome
execute hasTimeLimit cmd args dir =
 do { result <- (if hasTimeLimit then timeout (maxTimeInSeconds*1000000) else fmap Just) $ 
                  executeIO cmd args dir
    ; case result  of
        Just executionOutcome -> return executionOutcome
        Nothing -> return $ ExecFailure timeoutExitCode $ "Timeout: Execution time exceeded "++show maxTimeInSeconds++" seconds."
    }
executeIO :: String -> [String] -> String -> IO ExecutionOutcome
executeIO cmd args dir =
 do { let cp = CreateProcess
                { cmdspec       = RawCommand cmd args
                , cwd           = Just dir
                , env           = Nothing -- environment
                , std_in        = Inherit 
                , std_out       = CreatePipe
                , std_err       = CreatePipe
                , close_fds     = False -- no need to close all other file descriptors
                , create_group  = False
                , delegate_ctlc = False -- don't let Ampersand handle ctrl-c
                }
                 
--    ; putStrLn $ "Execute: "++cmd++" "++intercalate " " args ++ "   in "++dir      

    -- Use bracketOnError to kill the process on an exception. Otherwise processes that time out continue running.
    -- To keep things easy, we don't collect output in case of an exception. (not really necessary when testing 
    -- Ampersand, since tests can easily be reproduced)
    ; bracketOnError (createProcess cp)                           -- wait for process to prevent creation of zombies
                     (\(_,_,_,pHandle)-> terminateProcess pHandle >> waitForProcess pHandle) $
      \(_, mStdOut, mStdErr, pHandle) -> 
            case (mStdOut, mStdErr) of
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
                               ExitFailure c -> ExecFailure c $ "Exit code " ++ show c ++  -- include exit code in message, so we don't need to show it later
                                                                "\nstdout:\n"  ++ outputStr ++ -- also show stdout, since many programs
                                                                "\nstderr:\n"  ++ errStr    -- show errors on stdout instead of stderr
                  }
      
    }

