module TestSpecs where

import Types

-- These TestSpecs are only for experimentation. The actual test specs used by the sentinel are
-- dynamically read from Sentinel/scripts/TestSpecs.txt during testing

testTestSpecs :: [TestSpec]          
testTestSpecs = 
  [ TestSpec Ampersand [] ShouldSucceed 
      [ "Prototype/apps/Simple"
      , "Prototype/apps/Misc"
      , "Prototype/apps/Tests/ShouldSucceed"
--      , "Prototype/apps/INDOORS/INDOORS.adl"
      ]
  , TestSpec Prototype ["--validate"] ShouldSucceed 
      [ "Prototype/apps/Simple"
      , "Prototype/apps/Misc"
      , "Prototype/apps/Tests/ShouldSucceed"
--      , "Prototype/apps/INDOORS/INDOORS.adl"
      , "Prototype/apps/Bugs/SQL/Fixed"
      ]
  , TestSpec Ampersand [] ShouldFail
      [ "Prototype/apps/Tests/ShouldFail" 
      ]
  , TestSpec Ampersand [] ShouldFail
      [ "Prototype/apps/Tests/ShouldFail" 
      ]
  ]

