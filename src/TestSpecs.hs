module TestSpecs where

import Types

-- This TestSpecs module is only for experimentation and is currently not imported. 
-- The actual test specs used by the sentinel are dynamically read from Sentinel/www/TestSpecs.txt during testing

testTestSpecs :: [TestSpec]          
testTestSpecs = 
  [ TestSpec Ampersand [] [1] ShouldSucceed 
      [ "ampersand-models/Simple"
      , "ampersand-models/Misc"
      , "ampersand-models/Tests/ShouldSucceed"
--      , "ampersand-models/INDOORS/INDOORS.adl"
      ]
  , TestSpec Ampersand ["--haskell"] [1] ShouldSucceed 
      [ "ampersand-models/Simple"
      , "ampersand-models/Misc"
      , "ampersand-models/Tests/ShouldSucceed"
--      , "ampersand-models/INDOORS/INDOORS.adl"
      ]
  , TestSpec Prototype ["--validate"] [1] ShouldSucceed 
      [ "ampersand-models/Simple"
      , "ampersand-models/Misc"
      , "ampersand-models/Tests/ShouldSucceed"
--      , "ampersand-models/INDOORS/INDOORS.adl"
      , "ampersand-models/Bugs/SQL/Fixed"
      ]
  , TestSpec Ampersand [] [1] ShouldFail
      [ "ampersand-models/Tests/ShouldFail" 
      ]
  , TestSpec Ampersand [] [1] ShouldFail
      [ "ampersand-models/Tests/ShouldFail" 
      ]
  ]

