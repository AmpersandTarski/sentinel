module Main where

import System.IO
import Network.BSD
import System.Locale
import Data.Time
import Utils
import Test
import Execute

import TestSpecs

{-
todo:
put info about test in result
put message in test
read testSpecs instead of compile
send e-mail in case of error
fix dir structure with bug dirs
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
    
    ; isTestSrv <- isTestServer
    ; if isTestSrv -- allow different behavior on dedicated server and elsewhere for quick testing
      then
       do { svnUpdate "Ampersand"
          ; svnUpdate "Prototype"
          -- also update and build Sentinel? Or do we want to keep this an explicit action on the server?
          
          ; cabalClean "Ampersand" []
          ; reportResult $ testBuild "Ampersand" ["-f-library"] -- test building the executable
          ; reportResult $ testInstall "Ampersand" ["-f-executable"] -- test building the library
          ; cabalClean "Prototype" []
          ; reportResult $ testBuild "Prototype" []
          ; return ()
          }
      else
       do { return ()
          }
    
    ; mapM runTestSpec testSpecs
    
    ; exit
    }

initialize :: IO ()
initialize =
 do { hSetBuffering stdout LineBuffering
    ; hSetBuffering stderr LineBuffering
    ; hName <- getHostName
    ; time <- fmap (formatTime defaultTimeLocale "%-T %-d-%b-%y") getZonedTime
    ; putStrLn $ "######## Sentinel started on "++hName++" at "++time++" ########\n\n"
    }
    
exit :: IO ()
exit =
 do { time <- fmap (formatTime defaultTimeLocale "%-T %-d-%b-%y") getZonedTime
    ; putStrLn $ "######## Sentinel exited at "++time++" ########\n\n"
    }