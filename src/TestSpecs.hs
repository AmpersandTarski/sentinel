module TestSpecs where

import Types

testSpecs :: [TestSpec]          
testSpecs = 
  [ TestSpec Ampersand ShouldSucceed [ "Prototype/apps/Simple"
                                     , "Prototype/apps/Misc"
                                     , "Prototype/apps/Tests/ShouldSucceed"
                                     , "Prototype/apps/INDOORS/INDOORS.adl"
                                     ]
  ]

