module Main where

import Prelude hiding (catch)
import Control.Exception
import System.IO
import Network.BSD
import System.Locale
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
will also cause a lot of test fails.

fix module names and structure, as well as refactor test data types. Everything is rather messy now.
keep track of what has been reported
maybe create a type TestCase, a list of which is created from TestSpec
if the message is not in the result we can print it before executing the test
Also keep desired result in TestResult, so we can give a less confusing message (e.g. this test succeeded but should have failed)
and only print output in case of failed run, rather than a failed test (output from successful tests that should have failed is probably not interesting)

maybe even use something like Passed/NotPassed for tests instead of Success/Failure

maybe keep testfiles relative until test, so reporting is less verbose

collectFilePaths failures should be reported (but not internal ones, these should just fail (and won't occur))

configure fail should also be reported

copy all test failures to a www page for an easy overview

figure out what to do when we get dependency problems after cabal update

low priority:
take --enable-tests into account

-}

main :: IO ()
main = 
 do { initialize
    ; mFailureMessage <- performTests
    ; case mFailureMessage of
        Nothing -> return ()
        Just failureMessage ->
         do { authors <- getAuthors
            ; putStrLn $ "\n\nNotifying "++intercalate ", " authors
            ; notifyByMail authors "Test failure" $ 
                "This is an automated mail from the Ampersand Sentinel.\n\n" ++
                failureMessage ++ "\n\n"++
                "Please consult http://sentinel.oblomov.com/ampersand/SentinelOutput.txt for more details."
          } 
    ; exit
    }
    
performTests :: IO (Maybe String)
performTests =
 do { svnUpdate "Sentinel/scripts" -- update the scripts directory, so we get the most recent TestSpecs.txt
    ; testSpecs <- parseTestSpecs
    
    ; isTestSrv <- isTestServer
    ; buildTestResults <-
        if isTestSrv -- allow different behavior on dedicated server and elsewhere for quick testing
        then
         do { svnUpdate "Ampersand"
            ; svnUpdate "Prototype"
            -- also update and build Sentinel? Or do we want to keep this an explicit action on the server?
            
            ; cabalClean "Ampersand" []
            ; t1 <- reportTestResult $ testBuild "Ampersand" ["-f-library"]     "the Ampersand executable"
            ; t2 <- reportTestResult $ testInstall "Ampersand" ["-f-executable"] "the Ampersand library"
            ; cabalClean "Prototype" []
            ; t3 <- reportTestResult $ testBuild "Prototype" [] "the prototype generator"
            ; return [t1,t2,t3] -- TODO: probably want a monad here, since it's too easy to miss tests now
            }
        else
         do { cabalClean "Ampersand" []
            ; t1 <- reportTestResult $ testBuild "Ampersand" ["-f-library"]     "the Ampersand executable"
            ; t2 <- reportTestResult $ testInstall "Ampersand" ["-f-executable"] "the Ampersand library"
            ; cabalClean "Prototype" []
            ; t3 <- reportTestResult $ testBuild "Prototype" [] "the prototype generator"
            ; return [t1,t2,t3]
            }
    
    ; executionTestResults <- fmap concat $ mapM runTestSpec testSpecs
    ; let allTestResults = buildTestResults ++ executionTestResults
    
    ; let failedTestResults = filter (not . isTestSuccessful) allTestResults
          nrOfFailed = length failedTestResults
    
    ; putStrLn $ unlines [ "\n\n--------"
                         , "Total number of tests: " ++ show (length allTestResults)
                         , "Number of failed tests: " ++ show nrOfFailed
                         ]
    ; let failedTestsTxt = intercalate "\n\n" . map showTestResult $ failedTestResults
                                
    ; if nrOfFailed == 0
      then return Nothing
      else do { putStrLn failedTestsTxt
              ; return $ Just $ show nrOfFailed ++ " test"++(if nrOfFailed==1 then "" else "s")++
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
    ; putStrLn $ "######## Sentinel ("++revStr++") started on "++hName++" at "++time++" ########\n\n"
    }
    
exit :: IO ()
exit =
 do { time <- fmap (formatTime defaultTimeLocale "%-T %-d-%b-%y") getZonedTime
    ; putStrLn $ "\n######## Sentinel exited at "++time++" ########\n\n"
    }