module Execute 
   (testInstall,logExecutableVersion
   , execute,timeoutExitCode
   )
   where

import System.Process
import System.Timeout
import System.Exit
import System.IO
import System.FilePath 
import Control.Exception
import Data.List              
import Utils
import Defaults
import Types

maxTimeInSeconds :: Int
maxTimeInSeconds = 4 * 60 -- Timeout is only 2 minutes as ampersand is fast enough nowadays. We can increase it when we get unexpected timeouts 

timeoutExitCode :: Int
timeoutExitCode = -1 -- -1 is not used by unix processes



-- doing install without also building is not possible with stack.
testInstall :: String -> [String] -> String -> IO TestResult
testInstall project flags targetDescr = mkExecutionTest testDescr $
 do { putStrLn testDescr
    ; executeStack "install" project flags -- pass flags directly to install
    }
 where testDescr = "Building and installing "++targetDescr++". (project: "++project++", flags: ["++intercalate ", " flags++"]) {should succeed}"

--data StackCmd = Clean | Build | Install
  
logExecutableVersion :: IO ()
logExecutableVersion =
 do { isTestSrv <- isTestServer
    ; let executable = binDir isTestSrv ++ "/ampersand"
    ; result <- execute executable ["--version"] "."
    ; case result of
        ExecSuccess versionInfo -> putStrLn versionInfo
        ExecFailure _ err       -> putStrLn $ "Error while obtaining version for " ++ executable ++ ":\n" ++ err
    }                              -- This is not one of the tests, so we simply print the error
    
executeStack :: String -> String -> [String] -> IO ExecutionOutcome
executeStack cmd project flags =
 do { gitDir <- getGitDir
    ; execute "stack" (cmd : flags ) $ combine gitDir project
    }

execute :: String -> [String] -> String -> IO ExecutionOutcome
execute cmd args dir =
 do { result <- timeout (maxTimeInSeconds*1000000) $ executeIO cmd args dir
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
                , delegate_ctlc = False -- don't let child process handle ctrl-c
                }
                 
    ; putStrLn $ "Execute: "++cmd++" "++intercalate " " args ++ "   in "++dir      

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

