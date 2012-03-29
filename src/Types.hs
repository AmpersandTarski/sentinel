module Types where

type TestFailure = String
type TestSuccess = String

type TestResult = Either TestFailure TestSuccess

data DesiredOutcome = ShouldSucceed | ShouldFail deriving Show

data TestExecutable = Ampersand | Prototype deriving Show

data TestSpec = TestSpec { getExecutable :: TestExecutable 
                         , getDesiredOutcome :: DesiredOutcome
                         , getTestFileSpecs :: [String] -- relative to svn directory
                         } deriving Show
               