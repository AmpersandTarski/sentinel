module Defaults where

testServerHostname :: String
testServerHostname = "Sentinel"

gitPath :: Bool -> String
gitPath True  = "git"           -- path on dedicated test server
gitPath False = "git/Ampersand"

binDir :: Bool -> String
binDir True  = "/home/sentinel/.local/bin" -- path on dedicated test server
binDir False = "/Users/martijn/git/Ampersand/ampersand/.cabal-sandbox/bin/"

outputDir :: String
outputDir = "sentinel/www/ampersand"

testSpecsFile :: String
testSpecsFile = "sentinel/www/TestSpecs.txt"

authorsFile :: String
authorsFile = "sentinel/www/Authors.txt"

