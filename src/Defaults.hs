module Defaults where

testServerHostname :: String
testServerHostname = "Sentinel"

testServerSvnPath :: String
testServerSvnPath = "svn"

oblomovSvnPath :: String
oblomovSvnPath = "svn/Eclipse" -- used if we're not on testServer

testSpecsFile :: String
testSpecsFile = "Sentinel/www/TestSpecs.txt"

authorsFile :: String
authorsFile = "Sentinel/www/Authors.txt"

outputDir :: String
outputDir = "Sentinel/www/ampersand"

binDir = "/home/sentinel/.cabal/bin"
--binDir = "/Users/martijn/.cabal/bin"
