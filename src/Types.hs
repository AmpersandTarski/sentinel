module Types where

data TestResult = TestResult { getResultOutcome :: TestOutcome
                             , getResultSpec    :: TestSpec
                             } deriving Show

isTestSuccessful :: TestResult -> Bool
isTestSuccessful (TestResult (TestSuccess _) _) = True
isTestSuccessful _                              = False

data ExecutionOutcome = ExecFailure String | ExecSuccess String deriving Show
                             
data TestOutcome = TestFailure String | TestSuccess String deriving Show

data DesiredOutcome = ShouldFail | ShouldSucceed deriving Show
-- note that ShouldFail should not fail with a fatal

data TestExecutable = Ampersand | Prototype deriving Show

data TestSpec = TestSpec { getTestExecutable :: TestExecutable 
                         , getTestArgs       :: [String] 
                         , getDesiredOutcome :: DesiredOutcome
                         , getTestFileSpecs  :: [String] -- relative to svn directory
                         } deriving Show
               