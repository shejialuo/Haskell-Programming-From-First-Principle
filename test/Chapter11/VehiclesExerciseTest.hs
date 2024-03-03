module Chapter11.VehiclesExerciseTest where

import Chapter11.VehiclesExercise (areCars, doge, isCar, isPlane, myCar)
import Test.HUnit (Test (TestCase, TestList), assertEqual)

testIsCar :: Test
testIsCar =
  TestList
    [ TestCase $ do
        assertEqual
          "isCar myCar"
          True
          $ isCar myCar
        assertEqual
          "isCar doge"
          False
          $ isCar doge
    ]

testAreCars :: Test
testAreCars =
  TestList
    [ TestCase $ do
        assertEqual
          "areCars [myCar, doge]"
          [True, False]
          $ areCars [myCar, doge]
    ]

testIsPlane :: Test
testIsPlane =
  TestList
    [ TestCase $ do
        assertEqual
          "isPlane doge"
          True
          $ isPlane doge
        assertEqual
          "isPlane myCar"
          False
          $ isPlane myCar
    ]

testSuite :: Test
testSuite =
  TestList
    [ testIsCar,
      testIsPlane,
      testAreCars
    ]
