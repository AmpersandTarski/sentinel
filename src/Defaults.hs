module Defaults where

testServerHostname :: String
testServerHostname = "Sentinel"

testServerGitPath :: String
testServerGitPath = "git"

oblomovGitPath :: String
oblomovGitPath = "git/Ampersand" -- used if we're not on the dedicated test server

testSpecsFile :: String
testSpecsFile = "sentinel/www/TestSpecs.txt"

authorsFile :: String
authorsFile = "sentinel/www/Authors.txt"

outputDir :: String
outputDir = "sentinel/www/ampersand"

binDir :: String
binDir = "/home/sentinel/.cabal/bin"
--binDir = "/Users/martijn/.cabal/bin"
