module TestSpecs where

import Types

testSpecs :: [TestSpec]          
testSpecs = 
  [ TestSpec Ampersand [] ShouldSucceed 
      [ "Prototype/apps/Simple"
      , "Prototype/apps/Misc"
      , "Prototype/apps/Tests/ShouldSucceed"
      , "Prototype/apps/INDOORS/INDOORS.adl"
      ]
  , TestSpec Prototype ["--validate"] ShouldSucceed 
      [ "Prototype/apps/Simple"
      , "Prototype/apps/Misc"
      , "Prototype/apps/Tests/ShouldSucceed"
      , "Prototype/apps/INDOORS/INDOORS.adl"
      , "Prototype/apps/Bugs/SQL/Fixed"
      ]
  , TestSpec Ampersand [] ShouldFail
      [ "Prototype/apps/Tests/ShouldFail" 
      ]
  , TestSpec Ampersand [] ShouldFail
      [ "Prototype/apps/Tests/ShouldFail" 
      ]
  ]

