module Main where

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
import Defaults

{-
todo:

Avoid needing root and sentinel permissions for executing server.

fix module names and structure, as well as refactor test data types. Everything is rather messy now.
keep track of what has been reported
maybe create a type TestCase, a list of which is created from TestSpec
if the message is not in the result we can print it before executing the test
Also keep desired result in TestResult, so we can give a less confusing message (e.g. this test succeeded but should have failed)
and only print output in case of failed run, rather than a failed test (output from successful tests that should have failed is probably not interesting)

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
    ; (nrOfFailed, totalNrOfTests, mFailureMessage) <- performTests opts
    ; when (optMail opts) $
        case mFailureMessage of
          Nothing -> putStrLn "\n\nNo need to wake anybody by sending a mail, for there were no failures."
          Just failureMessage -> when isTestSrv $
           do { authors <- getAuthors
              ; putStrLn $ "\n\nNotifying "++intercalate ", " authors
              ; notifyByMail authors ("Test failure (" ++ show nrOfFailed ++ "/" ++ show totalNrOfTests ++ ")") $ 
                  "This is an automated mail from the Ampersand Sentinel.\n\n" ++
                  failureMessage ++ "\n\n"++
                  "Please consult http://sentinel.tarski.nl for more details."
              } 
    ; logExit
    }

performTests :: Options -> IO (Int, Int, Maybe String)
performTests opts =
 do { testSpecs <- parseTestSpecs opts
 
    ; isTestSrv <- isTestServer
    ; buildTestResults <-
        if isTestSrv -- allow different behavior on dedicated server and elsewhere for quick testing
        then
         do { res <- reportTestResult opts $ testInstall "ampersand" ["--bindir="++binDir isTestSrv] "the Ampersand compiler"
            
            ; return [res] 
            }
        else
          return []

    -- sort test based on fSpec/prototype generation.
    ; execTests <- fmap concat $ mapM (createTests opts) $ 
                      getTestSpecsForExecutable testSpecs Ampersand ++
                      getTestSpecsForExecutable testSpecs Prototype
    
    ; executionTestResults <- if all isTestSuccessful buildTestResults
                              then do { putStrLn "Version of executable used for testing:"
                                      ; logExecutableVersion
                                      ; sequence execTests
                                      }
                              else return []
        
    ; let allTestResults = buildTestResults ++ executionTestResults
    
    ; let failedTestResults = filter (not . isTestSuccessful) allTestResults
          nrOfFailed = length failedTestResults
          totalNrOfTests = length allTestResults 
    -- nrOfFailed and totalNrOfTests only include tests that were actually run. We could also use the total number of specified tests as a base,
    -- but then a compile fail would suggest a large number of failed tests.
    
    ; putStr $ if optHtml opts then "<hr/>" else "\n\n--------"
    ; putStrLn $ bracketHtml opts "<p style='font-weight: bold'>\n" "</p>" $
                   unlines [ "Total number of tests: " ++ show totalNrOfTests          -- NOTE: scripts/runSentinel depends on the exact
                           , "Number of failed tests: " ++ show nrOfFailed             --       format of these two lines.
                           ]

    ; mFailureMessage <- 
        if nrOfFailed == 0
        then return Nothing
        else do { let failedTestsTxt = intercalate "\n\n" . map showTestResult $ failedTestResults
              
                ; putStrLn $ "\nList of tests that did not pass\n\n" ++ failedTestsTxt
                ; return $ Just $ show nrOfFailed ++ " test"++(if nrOfFailed==1 then "" else "s")++
                                  " out of "++ show (length allTestResults)++
                                  " did not pass:\n\n"++
                                  failedTestsTxt
                } 
    ; return (nrOfFailed, totalNrOfTests, mFailureMessage)   
    } `catch` \e -> do { let message = 
                               "An unexpected exception has occurred during Sentinel execution.\n" ++
                               show (e :: SomeException) ++ "\nTesting was aborted."
                       ; putStrLn $ "\n\nERROR: " ++ message
                       ; return (0,0, Just message)
                       }
                       
initialize :: IO ()
initialize =
 do { hSetBuffering stdout LineBuffering
    ; hSetBuffering stderr LineBuffering
    ; hName <- getHostName
    ; time <- fmap (formatTime defaultTimeLocale "%-T %-d-%b-%y") getZonedTime
    ; putStrLn $ "######## Sentinel started on "++hName++" at "++time++" ########\n\n"
    }
    
logExit :: IO ()
logExit =
 do { time <- fmap (formatTime defaultTimeLocale "%-T %-d-%b-%y") getZonedTime
    ; putStrLn $ "\n######## Sentinel exited at "++time++" ########\n\n" -- NOTE: www/index.php depends on the exact format of this line.
    }
