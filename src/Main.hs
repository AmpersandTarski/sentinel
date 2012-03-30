module Main where

import System.IO
import Network.BSD
import System.Locale
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
send e-mail in case of error
fix module names and structure


catch errors and report
keep track of what has been reported
maybe create a type TestCase, a list of which is created from TestSpec
maybe keep testfiles relative until test, so reporting is less verbose

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
    
    ; svnUpdate "Sentinel/scripts" -- update the scripts directory, so we get the most recent TestSpecs.txt
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
       do { return ()
          }
    
    ; testResults <- fmap concat $ mapM runTestSpec testSpecs
    ; let failedTestResults = filter (not . isTestSuccessful) testResults
    ; putStrLn $ unlines [ "\n\n--------"
                         , "Total number of tests: " ++ show (length testResults)
                         , "Number of failed tests: " ++ show (length failedTestResults)
                         ]
    ; exit
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
    ; putStrLn $ "######## Sentinel exited at "++time++" ########\n\n"
    }