module Test where

import Data.Char
import Data.List
import System.Directory hiding (executable)
import System.FilePath               
import Types
import Utils
import Execute
import Defaults
import UTF8(readFile,putStrLn)
import Prelude hiding (writeFile,readFile,getContents,putStr,putStrLn)

createTests :: Options -> TestSpec -> IO [IO TestResult]
createTests opts testSpec =
 do { testFiles <- getTestFiles $ getTestFileSpecs testSpec
    ; return $ map (reportTestResult opts . runTest opts testSpec) testFiles
    }

getTestFiles :: [String] -> IO [String]   
getTestFiles fileSpecs =
 do { gitDir <- getGitDir
    ; let absFileSpecs = map (combine gitDir) fileSpecs
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
          ; fmap concat (mapM (collectFilePaths . combine absFileSpec) filesOrDirs)
          }
    }
   

-- execute project executable in directory of testFile (which is absolute). First arg is testFile
-- an implicit --outputDir/--proto is added for  gitDir / outputDir / filename ( and /fSpec for Ampersand) 
-- Because the prototype generator does not respect outputDir, we need to use protoDir, which also forces
-- generation of a prototype. However, this implicit argument will be replaced by a more general mechanism to
-- allow filename to be used in the test spec (e.g. ["--outputDir=$OUTPUTDIR/$FILENAME/fSpec"])
runTest :: Options -> TestSpec -> FilePath -> IO TestResult
runTest opts testSpec@(TestSpec exec args panicExitCodes desOutcome _) testFile =
 do { putStrLn testDescr
    ; gitDir <- getGitDir
    ; isTestSrv <- isTestServer  
    ; let executable = binDir isTestSrv ++ "/ampersand"
    ; let absOutputDir = joinPath $ [gitDir, outputDir, dropExtension (takeFileName testFile)] ++
                                    ["fSpec" | exec == Ampersand] 
          absOutputDirArg = (if exec == Ampersand then "--outputDir=" else "--proto=") ++ absOutputDir
    ; result <- execute executable (testFile : absOutputDirArg 
                                   : args ++ ["+RTS", "-M4G"]) $ takeDirectory testFile 
                                   -- We set the max heap size to 4 Gb.
    ; case result of
        ExecSuccess _   -> putStrLn   "Execution success"
        ExecFailure exitCode err -> putStrLn $ (if isPanicExitCode exitCode 
                                                then "Execution exception (exit code is in panicExitCodes): " 
                                                else "Execution failure: ") ++
                                               bracketHtml opts "<div style='font-family:courier; font-size:85%'>" "</div>" err
                         
    ; let testOutcome = 
                 case (result, getDesiredOutcome testSpec) of
                   (ExecFailure exitCode err, _         ) | isPanicExitCode exitCode -> TestFailure err
                   (ExecFailure _ err,        ShouldFail)    -> TestSuccess err
                   (ExecFailure _ err,        ShouldSucceed) -> TestFailure err
                   (ExecSuccess outp,         ShouldFail)    -> TestFailure outp
                   (ExecSuccess outp,         ShouldSucceed) -> TestSuccess outp
    ; return $ TestResult testOutcome testDescr
    }
 where testDescr = "Running "++show exec++" "++show args++" on "++testFile ++ ". {"++showDesiredOutcome desOutcome ++"}"
       
       isPanicExitCode exitCode = exitCode `elem` timeoutExitCode:panicExitCodes -- timeout always yields a panic
       
-- yes, parse is a rather big word for this function
parseTestSpecs :: Options -> IO [TestSpec]
parseTestSpecs opts =
 do { gitDir <- getGitDir
    ; let testSpecsFilePath = combine gitDir testSpecsFile
    ; testSpecsStr <- readFile testSpecsFilePath
    ; putStrLn $ "Contents of " ++ testSpecsFile ++ ":"
    ; putStrLn $ bracketHtml opts "<div style='font-family: courier; font-size:85%; color:blue'>" "</div>" $
                   "\n" ++ testSpecsStr ++ "\n\n"
    ; let lexedTestSpecsStr = reverse . dropWhile isSpace . reverse -- read doesn't like trailing whitespace 
                            . unlines . filter (not . ("--" `isPrefixOf`)) . lines   -- line comments are only allowed at start of line 
                            $ testSpecsStr                          -- (otherwise we also need to escape strings for "--validate")
    ; case readMaybe lexedTestSpecsStr :: Maybe [TestSpec] of
        Nothing  -> error $ "ERROR: cannot read file " ++ testSpecsFilePath ++
                            "\n\nMake sure comments in TestSpecs.txt are at the start of the line," ++ 
                            " otherwise they are interpreted as options (e.g. 'getTestArgs = [\"--haskell\"]').\n"
        Just tss -> return tss
    }
