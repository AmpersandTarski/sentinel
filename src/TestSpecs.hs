module TestSpecs where

import Types

-- This TestSpecs module is only for experimentation and is currently not imported. 
-- The actual test specs used by the sentinel are dynamically read from Sentinel/scripts/TestSpecs.txt during testing

testTestSpecs :: [TestSpec]          
testTestSpecs = 
  [ TestSpec Ampersand [] [1] ShouldSucceed 
      [ "Prototype/apps/Simple"
      , "Prototype/apps/Misc"
      , "Prototype/apps/Tests/ShouldSucceed"
--      , "Prototype/apps/INDOORS/INDOORS.adl"
      ]
  , TestSpec Prototype ["--validate"] [1] ShouldSucceed 
      [ "Prototype/apps/Simple"
      , "Prototype/apps/Misc"
      , "Prototype/apps/Tests/ShouldSucceed"
--      , "Prototype/apps/INDOORS/INDOORS.adl"
      , "Prototype/apps/Bugs/SQL/Fixed"
      ]
  , TestSpec Ampersand [] [1] ShouldFail
      [ "Prototype/apps/Tests/ShouldFail" 
      ]
  , TestSpec Ampersand [] [1] ShouldFail
      [ "Prototype/apps/Tests/ShouldFail" 
      ]
  ]

