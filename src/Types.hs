module Types where

type TestFailure = String
type TestSuccess = String

type TestResult = Either TestFailure TestSuccess

data DesiredOutcome = ShouldSucceed | ShouldFail deriving Show
-- note that ShouldFail should not fail with a fatal

data TestExecutable = Ampersand | Prototype deriving Show

data TestSpec = TestSpec { getTestExecutable :: TestExecutable 
                         , getTestArgs       :: [String] 
                         , getDesiredOutcome :: DesiredOutcome
                         , getTestFileSpecs  :: [String] -- relative to svn directory
                         } deriving Show
               