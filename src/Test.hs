module Test where

import Data.Char
import System.Directory hiding (executable)
import System.FilePath               
import Types
import Utils
import Execute


runTestSpec :: TestSpec -> IO ()
runTestSpec (TestSpec exec outcome fileSpecs) =
 do { testFiles <- getTestFiles fileSpecs
    ; sequence_ $ map (reportResult . runTest exec []) testFiles
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
runTest :: TestExecutable -> [String] -> FilePath -> IO TestResult
runTest exec args testFile =
 do { putStrLn $ "Testing "++show exec++" "++show args++" on "++testFile
    ; svnDir <- getSvnDir
    ; let executable =
            case exec of
              Ampersand -> svnDir ++ "/Ampersand/dist/build/ampersand/ampersand"
              Prototype -> svnDir ++ "/Prototype/dist/build/prototype/prototype"
    ; result <- execute executable (testFile : args) $ takeDirectory testFile 
    ; return result 
    }
