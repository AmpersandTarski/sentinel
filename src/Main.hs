module Main where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import System.IO
import Network.BSD
import System.Locale
import Data.List
import Data.Time
import Types
import Utils
import Test
import Execute

import TestSpecs

{-
todo:


fix how execution tests are put in testResult now (see "todo: this is wrong")
put message in test
fix module names and structure
keep track of what has been reported
maybe create a type TestCase, a list of which is created from TestSpec
maybe keep testfiles relative until test, so reporting is less verbose


Make scripts more robust and allow svn update without first having to remove everything.
Currently we need to clean everything every time, so an Ampersand or Prototype compilation fail
will also cause a lot of test fails.

different types for testResult and executionResult
collectFilePaths failures should be reported (but not internal ones, these should just fail (and won't occur))

configure fail should also be reported

copy all test failures to a www page for an easy overview

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
                "This is an automated mail from the Ampersand Sentinel\n\n" ++
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
    ; if isTestSrv -- allow different behavior on dedicated server and elsewhere for quick testing
      then
       do { svnUpdate "Ampersand"
          ; svnUpdate "Prototype"
          -- also update and build Sentinel? Or do we want to keep this an explicit action on the server?
          
          ; cabalClean "Ampersand" []
          ; reportTestResult $ testBuild "Ampersand" ["-f-library"] -- test building the executable
          ; reportTestResult $ testInstall "Ampersand" ["-f-executable"] -- test building the library
          ; cabalClean "Prototype" []
          ; reportTestResult $ testBuild "Prototype" []
          ; return ()
          }
      else
       do { --reportTestResult $ testBuild "Ampersand" ["-f-library"] -- test building the executable
          ; return ()
          }
    
    ; testResults <- fmap concat $ mapM runTestSpec testSpecs
    ; let failedTestResults = filter (not . isTestSuccessful) testResults
          nrOfFailed = length failedTestResults
    ; putStrLn $ unlines [ "\n\n--------"
                         , "Total number of tests: " ++ show (length testResults)
                         , "Number of failed tests: " ++ show nrOfFailed
                         ]
                         
    ; return $ if nrOfFailed == 0
               then Nothing
               else Just $ show nrOfFailed ++ " test"++(if nrOfFailed==1 then "" else "s")++" have failed."
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