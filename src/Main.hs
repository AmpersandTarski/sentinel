module Main where

import System.IO
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
 do { hSetBuffering stdout LineBuffering
    ; hSetBuffering stderr LineBuffering
    
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
  
