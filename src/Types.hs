module Types where

import Control.Applicative
import Prelude -- to silence redundant Control.Applicative import for GHC >= 7.10
import Options hiding (Options)
import qualified Options

data TestResult = TestResult { getResultOutcome :: TestOutcome
                             , getResultTestDescr :: String
                             } deriving Show

showTestResult :: TestResult -> String
showTestResult (TestResult _ descr) = "Test: " ++ descr 
       
isTestSuccessful :: TestResult -> Bool
isTestSuccessful (TestResult (TestSuccess _) _) = True
isTestSuccessful _                              = False

data ExecutionOutcome = ExecFailure Int String | ExecSuccess String deriving Show
                             
data TestOutcome = TestFailure String | TestSuccess String deriving (Show, Eq)

data DesiredOutcome = ShouldFail | ShouldSucceed deriving (Show, Read)
-- note that a ShouldFail test should not fail with a fatal

showDesiredOutcome :: DesiredOutcome -> String
showDesiredOutcome ShouldSucceed = "should succeed"
showDesiredOutcome ShouldFail    = "should fail"

data TestExecutable = Ampersand | Prototype deriving (Show, Read, Eq)

data TestSpec = TestSpec { getTestExecutable :: TestExecutable 
                         , getTestArgs       :: [String] 
                         , getPanicExitCodes :: [Int]    -- exit codes for which the TestOutcome will always be TestFailure, 
                                                         -- even when the DesiredOutcome is ShouldFail
                         , getDesiredOutcome :: DesiredOutcome
                         , getTestFileSpecs  :: [String] -- relative to git directory
                         } deriving (Show, Read)

getTestSpecsForExecutable :: [TestSpec] -> TestExecutable -> [TestSpec]
getTestSpecsForExecutable testSpecs executable =
  filter ((==executable) . getTestExecutable) testSpecs

data Options = Options
    { optHtml :: Bool
    , optMail :: Bool
    , optDeleteSandbox :: Bool
    }

instance Options.Options Options where
    defineOptions = pure Options
        <*> simpleOption "html"          False "Generate html output."
        <*> simpleOption "mail"          False "Notify authors by e-mail."
        <*> simpleOption "deleteSandbox" False "Unused option: sandbox is handled by runSentinel"
                             