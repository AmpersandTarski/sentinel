module Main where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import System.IO
import Network.BSD
import System.Locale
import Options (runCommand)
import Data.List
import Data.Time
import Types
import Utils
import Test
import Execute

{-
todo:

Avoid needing root and martijn permissions for executing server.
Make scripts more robust and allow svn update without first having to remove everything.
Currently we need to clean everything every time, so an Ampersand or Prototype compilation fail
will also cause a lot of test fails. (there is an ugly fix for this now)
Maybe we should even manually keep old versions of the executables, since doing things incremental might be more fragile

fix module names and structure, as well as refactor test data types. Everything is rather messy now.
keep track of what has been reported
maybe create a type TestCase, a list of which is created from TestSpec
if the message is not in the result we can print it before executing the test
Also keep desired result in TestResult, so we can give a less confusing message (e.g. this test succeeded but should have failed)
and only print output in case of failed run, rather than a failed test (output from successful tests that should have failed is probably not interesting)

Use a Haskell server, so calling svn and cabal will be much easier. (Sentinel installation will also be easier).

Fix hanging of sentinel when Ampersand hangs (for example for bug #327)

check test specs before building anything, so you see quickly when there is a mistake

maybe even use something like Passed/NotPassed for tests instead of Success/Failure

maybe keep testfiles relative until test, so reporting is less verbose

collectFilePaths failures should be reported (but not internal ones, these should just fail (and won't occur))

configure fail should also be reported

copy all test failures to a www page for an easy overview

figure out what to do when we get dependency problems after cabal update (probably should use cabal-dev instead of cabal, to sandbox builds)

low priority:
use class tags and css instead of hard-coded styles
take --enable-tests into account

Send a notification if the server does not terminate within a certain period of time.
On one dodgy ghc configuration, cabal install somehow hangs when called from Sentinel.hs (doesn't seem to be waiting for input). 
This would not be signaled by the server, so we need a safe guard.

-}

 
main :: IO ()
main = runCommand $ \opts _ ->
 do { initialize
    ; isTestSrv <- isTestServer
    ; mFailureMessage <- performTests opts
    ; when (optMail opts) $
        case mFailureMessage of
          Nothing -> return ()
          Just failureMessage -> when isTestSrv $
           do { authors <- getAuthors
              ; putStrLn $ "\n\nNotifying "++intercalate ", " authors
              ; notifyByMail authors "Test failure" $ 
                  "This is an automated mail from the Ampersand Sentinel.\n\n" ++
                  failureMessage ++ "\n\n"++
                  "Please consult http://sentinel.tarski.nl for more details."
              } 
    ; exit
    }

performTests :: Options -> IO (Maybe String)
performTests opts =
 do { svnUpdate "Sentinel/www" -- update the www directory for the latest Authors.txt and TestSpecs.txt
    ; testSpecs <- parseTestSpecs opts
 
    ; isTestSrv <- isTestServer
    ; (ampersandOk, prototypeOk, buildTestResults) <-
        if isTestSrv -- allow different behavior on dedicated server and elsewhere for quick testing
        then
         do { putStrLn "Performing svn update for Ampersand"
            ; svnUpdate "Ampersand"
            ; putStrLn "Performing svn update for Prototype"
            ; svnUpdate "Prototype"
            -- also update and build Sentinel? Or do we want to keep this an explicit action on the server?
            ; putStrLn "Performing cabal update"
            ; cabalUpdate
            ; putStrLn "Cleaning Ampersand"
            ; cabalClean "Ampersand" []
            ; t2 <- reportTestResult opts $ testInstall "Ampersand" ["-f-executable"] "the Ampersand library"
            ; t1 <- reportTestResult opts $ testInstall "Ampersand" []                "the Ampersand executable (and library)" -- cannot build exec without lib because of in-place dependency
            ; putStrLn "Cleaning Prototype"
            ; cabalClean "Prototype" []
            ; t3 <- reportTestResult opts $ testInstall "Prototype" [] "the prototype generator"
            ; return ( isTestSuccessful t1, isTestSuccessful t3, [t1,t2,t3]) 
            -- TODO: probably want a monad here, since it's too easy to miss tests now
            }
        else
         do { --svnUpdate "Ampersand"
            ; --svnUpdate "Prototype"
            ; --cabalClean "Ampersand" []
            ; --t1 <- reportTestResult $ testBuild "Ampersand" ["-f-library"]      "the Ampersand executable"
            ; --t2 <- reportTestResult $ testInstall "Ampersand" ["-f-executable"] "the Ampersand library"
            ; --cabalClean "Prototype" []
            ; --t3 <- reportTestResult $ testBuild "Prototype" [] "the prototype generator"
            ; --return ( isTestSuccessful t1, isTestSuccessful t3, [t1,t2,t3]) 
            ; return (True, True, [])
            }
            
            
    ; let getTestResultsFor _          False = return []
          getTestResultsFor executable True  =
            fmap concat $ mapM (runTestSpec opts) [ ts | ts <- testSpecs, getTestExecutable ts == executable ]
    -- only execute tests if building the executable succeeded
    -- not very elegant, but only here until we can use the old executables in case of build failures
    ; ampersandExecTestResults <- getTestResultsFor Ampersand ampersandOk
    ; prototypeExecTestResults <- getTestResultsFor Prototype prototypeOk
    ; let executionTestResults = ampersandExecTestResults ++ prototypeExecTestResults
    ; let allTestResults = buildTestResults ++ executionTestResults
    
    ; let failedTestResults = filter (not . isTestSuccessful) allTestResults
          nrOfFailed = length failedTestResults
    
    ; putStr $ if optHtml opts then "<hr/>" else "\n\n--------"
    ; putStrLn $ bracketHtml opts "<p style='font-weight: bold'>" "</p>" $
                   unlines [ "Total number of tests: " ++ show (length allTestResults) -- NOTE: scripts/runSentinel depends on the exact
                           , "Number of failed tests: " ++ show nrOfFailed             --       format of these two lines.
                           ]

    ; if nrOfFailed == 0
      then return Nothing
      else do { let failedTestsTxt = intercalate "\n\n" . map showTestResult $ failedTestResults
              
              ; putStrLn $ "\nList of tests that did not pass\n\n" ++ failedTestsTxt
              ; return $ Just $ show nrOfFailed ++ " test"++(if nrOfFailed==1 then "" else "s")++
                                " out of "++ show (length allTestResults)++
                                " did not pass:\n\n"++
                                failedTestsTxt
              }    
    } `catch` \e -> do { let message = 
                               "An unexpected exception has occurred during Sentinel execution.\n" ++
                               show (e :: SomeException) ++ "\nTesting was aborted."
                       ; putStrLn $ "\n\nERROR: " ++ message
                       ; return $ Just message
                       }
                       
initialize :: IO ()
initialize =
 do { hSetBuffering stdout LineBuffering
    ; hSetBuffering stderr LineBuffering
    ; revStr <- getRevisionStr "Sentinel"
    ; hName <- getHostName
    ; time <- fmap (formatTime defaultTimeLocale "%-T %-d-%b-%y") getZonedTime
    ; putStrLn $ "######## Sentinel (r"++revStr++") started on "++hName++" at "++time++" ########\n\n"
    }
    
exit :: IO ()
exit =
 do { time <- fmap (formatTime defaultTimeLocale "%-T %-d-%b-%y") getZonedTime
    ; putStrLn $ "\n######## Sentinel exited at "++time++" ########\n\n" -- NOTE: www/index.php depends on the exact format of this line.
    }