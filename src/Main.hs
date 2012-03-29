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
maybe put info about test in result
maybe keep testfiles relative until test, so reporting is less verbose

collectFilePaths failures should be reported (but not internal ones, these should just fail (and won't occur))

configure fail should also be reported

copy all test failures to a www page for an easy overview

low priority:
take --enable-tests into account

-}
main :: IO ()
main =
 do { initialize
    {-
    ; cabalClean "Ampersand" []
    ; reportResult $ testBuild "Ampersand" ["-f-library"] -- test building the executable
    ; reportResult $ testInstall "Ampersand" ["-f-executable"] -- test building the library
    ; cabalClean "Prototype" []
    ; reportResult $ testBuild "Prototype" []
    -}
    
    ; mapM runTestSpec testSpecs
    
    ; return ()
    }

initialize :: IO ()
initialize =
 do { hSetBuffering stdout LineBuffering
    ; hSetBuffering stderr LineBuffering
    ; hName <- getHostName
    ; ct <- getCurrentTime
    ; let time = formatTime defaultTimeLocale "%-T %-d-%b-%y" ct
    ; putStrLn $ "######## Sentinel started on "++show hName++" at "++time++" ########\n\n"
    }
