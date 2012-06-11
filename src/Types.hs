{-# LANGUAGE TemplateHaskell #-}
module Types where

import Options hiding (Options)

data TestResult = TestResult { getResultOutcome :: TestOutcome
                             , getResultTestDescr :: String
                             } deriving Show

showTestResult :: TestResult -> String
showTestResult (TestResult _ descr) = "Test: " ++ descr 
       
isTestSuccessful :: TestResult -> Bool
isTestSuccessful (TestResult (TestSuccess _) _) = True
isTestSuccessful _                              = False

data ExecutionOutcome = ExecFailure String | ExecSuccess String deriving Show
                             
data TestOutcome = TestFailure String | TestSuccess String deriving (Show, Eq)

data DesiredOutcome = ShouldFail | ShouldSucceed deriving (Show, Read)
-- note that a ShouldFail test should not fail with a fatal

showDesiredOutcome :: DesiredOutcome -> String
showDesiredOutcome ShouldSucceed = "should succeed"
showDesiredOutcome ShouldFail    = "should fail"

data TestExecutable = Ampersand | Prototype deriving (Show, Read, Eq)

data TestSpec = TestSpec { getTestExecutable :: TestExecutable 
                         , getTestArgs       :: [String] 
                         , getDesiredOutcome :: DesiredOutcome
                         , getTestFileSpecs  :: [String] -- relative to svn directory
                         } deriving (Show, Read)


defineOptions "Options" $ do { boolOption "optHtml" "html" False "Generate html output."
                             ; boolOption "optMail" "mail" False "Notify authors by e-mail."
                             }
          