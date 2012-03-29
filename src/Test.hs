module Test where

import System.Directory hiding (executable)
import System.FilePath               
import Utils

getTestFiles :: [String] -> IO [String]   
getTestFiles fileSpecs =
 do { svnDir <- getSvnDir
    ; let absFileSpecs = map (combine svnDir) fileSpecs
    ; filePathss <- mapM collectFilePaths absFileSpecs
    ; return $ concat filePathss
    }

-- need extra indirection if we want single-file filespecs for test files with other extension than .adl
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
   
getProperDirectoryContents pth = fmap (filter (`notElem` [".","..",".svn"])) $ getDirectoryContents pth

type TestFailure = String
type TestSuccess = String

type TestResult = Either TestFailure TestSuccess

failOnError :: String -> IO TestResult -> IO ()
failOnError errMsg test =
 do { result <- test
    ; case result of
        Right _  -> return ()
        Left err -> error $ errMsg ++ err
    }
    
reportResult :: IO TestResult -> IO ()
reportResult test =
 do { result <- test
    ; case result of
        Right outp -> putStrLn $ "Success: " {- ++ outp -} ++ "\n\n\n" 
        Left err ->   putStrLn $ "Failure: "++err++"\n\n\n" 
    } 
    
