module Test where

import Data.Char
import Data.List
import System.Directory hiding (executable)
import System.FilePath               
import Types
import Utils
import Execute
import Defaults

runTestSpec :: Options -> TestSpec -> IO [TestResult]
runTestSpec opts testSpec =
 do { testFiles <- getTestFiles $ getTestFileSpecs testSpec
    ; mapM (reportTestResult opts . runTest opts testSpec) testFiles
    }
{-
    ; testFiles <- getTestFiles ["Prototype/apps/Simple", "Prototype/apps/Misc"]
    ; sequence_ $ map (reportResult . runTest "Ampersand" []) testFiles
    ; sequence_ $ map (reportResult . runTest "Prototype" []) testFiles

-}
getTestFiles :: [String] -> IO [String]   
getTestFiles fileSpecs =
 do { svnDir <- getSvnDir
    ; let absFileSpecs = map (combine svnDir) fileSpecs
    ; filePathss <- mapM collectFilePaths absFileSpecs
    ; return $ concat filePathss
    }

-- need extra indirection if we want single-file filespecs for test files with other extension than .adl
collectFilePaths :: String -> IO [FilePath]
collectFilePaths absFileSpec =
 do { dirExists <- doesDirectoryExist absFileSpec
    ; if not dirExists 
      then
       do { fileExists <- doesFileExist absFileSpec
          ; if not fileExists 
            then error $ "Incorrect test spec: "++absFileSpec 
            else return $ if takeExtension absFileSpec /= ".adl" then [] else [absFileSpec]
          }
      else
       do { filesOrDirs <- getProperDirectoryContents absFileSpec
          ; fmap concat $ mapM (\fOrD -> collectFilePaths (combine absFileSpec fOrD)) filesOrDirs
          }
    }
   

-- execute project executable in directory of testFile (which is absolute). First arg is testFile
-- an implicit --outputDir/--proto is added for  $svnDir / outputDir / filename ( and /fSpec for Ampersand) 
-- Because the prototype generator does not respect outputDir, we need to use protoDir, which also forces
-- generation of a prototype. However, this implicit argument will be replaced by a more general mechanism to
-- allow filename to be used in the test spec (e.g. ["--outputDir=$OUTPUTDIR/$FILENAME/fSpec"])
runTest :: Options -> TestSpec -> FilePath -> IO TestResult
runTest opts testSpec@(TestSpec exec args panicExitCodes desOutcome _) testFile =
 do { putStrLn $ testDescr
    ; svnDir <- getSvnDir
    ; let executable =
            case exec of
              Ampersand -> svnDir ++ "/Ampersand/dist/build/ampersand/ampersand"
              Prototype -> svnDir ++ "/Prototype/dist/build/prototype/prototype"
    ; let absOutputDir = joinPath $ [svnDir, outputDir, dropExtension (takeFileName testFile)] ++
                                    if exec == Ampersand then ["fSpec"] else []
          absOutputDirArg = (if exec == Ampersand then "--outputDir=" else "--proto=") ++ absOutputDir
    ; result <- execute executable (testFile : absOutputDirArg 
                                   : args) $ takeDirectory testFile 
    
    ; case result of
        ExecSuccess _   -> putStrLn   "Execution success"
        ExecFailure exitCode err -> putStrLn $ (if exitCode `elem` panicExitCodes 
                                                then "Execution exception (exit code is in panicExitCodes): " 
                                                else "Execution failure: ") ++
                                               bracketHtml opts "<div style='font-family:courier; font-size:85%'>" "</div>" err
                         
    ; let testOutcome = 
                 case (result, getDesiredOutcome testSpec) of
                   (ExecFailure exitCode err, _         ) | exitCode `elem` panicExitCodes -> TestFailure err
                   (ExecFailure _ err,        ShouldFail)    -> TestSuccess err
                   (ExecFailure _ err,        ShouldSucceed) -> TestFailure err
                   (ExecSuccess outp,         ShouldFail)    -> TestFailure outp
                   (ExecSuccess outp,         ShouldSucceed) -> TestSuccess outp
    ; return $ TestResult testOutcome testDescr
    }
 where testDescr = "Running "++show exec++" "++show args++" on "++testFile ++ ". {"++showDesiredOutcome desOutcome ++"}"
-- yes, parse is a rather big word for this function
parseTestSpecs :: Options -> IO [TestSpec]
parseTestSpecs opts =
 do { svnDir <- getSvnDir
    ; let testSpecsFilePath = combine svnDir testSpecsFile
    ; testSpecsStr <- readFile testSpecsFilePath
    ; let lexedTestSpecsStr = reverse . dropWhile isSpace . reverse -- read doesn't like trailing whitespace 
                            . unlines . filter (not . ("--" `isPrefixOf`)) . lines   -- line comments are only allowed at start of line 
                            $ testSpecsStr                          -- (otherwise we also need to escape strings for "--validate")
    ; putStrLn $ bracketHtml opts "<div style='font-family: courier; font-size:85%; color:blue'>" "</div>" $
                   "Parsing test specs:\n" ++ lexedTestSpecsStr ++ "\n\n"
    ; case readMaybe lexedTestSpecsStr :: Maybe [TestSpec] of
        Nothing  -> error $ "ERROR: cannot parse file "++testSpecsFilePath
        Just tss -> return tss
    }
