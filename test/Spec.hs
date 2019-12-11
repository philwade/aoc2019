import Test.Hspec
import DayOne as DayOne
import DayTwo as DayTwo
import DayThree as DayThree

main :: IO ()
main = hspec $ do
        describe "day one part one" $ do
            it "should get 2 from 12" $
              DayOne.getFuel 12 `shouldBe` 2
            it "should get 2 from 14" $
              DayOne.getFuel 14 `shouldBe` 2
            it "should get 654 from 1969" $
              DayOne.getFuel 1969 `shouldBe` 654
            it "should get 33583 from 100756" $
              DayOne.getFuel 100756 `shouldBe` 33583
            it "should get 4 for 12 and 14 together" $
              DayOne.runAll [12, 14] `shouldBe` 4
        describe "day one part two" $ do
            it "should get 2 from 14" $
              DayOne.getDoubleFuel 14 `shouldBe` 2
            it "should get 966 from 1969" $
              DayOne.getDoubleFuel 1969 `shouldBe` 966
            it "should get 50346 from 100756" $
              DayOne.getDoubleFuel 100756 `shouldBe` 50346
            it "should get 4 from two 14s" $
              DayOne.sumDouble [14, 14] `shouldBe` 4
        describe "day two basics" $ do
            it "should get an index" $
                DayTwo.returnIndex 2 [1, 2, 3, 4] `shouldBe` 3
            it "should set and index" $
                DayTwo.setIndex 2 2 [1, 2, 3, 4] `shouldBe` [1, 2, 2, 4]
        describe "day two part one" $ do
            it "should update programs" $
                DayTwo.runProgram [1,0,0,0,99] `shouldBe` [2,0,0,0,99]
            it "should update programs" $
                DayTwo.runProgram [2,3,0,3,99] `shouldBe` [2,3,0,6,99]
            it "should update programs" $
                DayTwo.runProgram [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]
            it "should update programs" $
                DayTwo.runProgram [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]
        describe "day three basics" $ do
            it "should map to the type" $
                DayThree.parseMoves ["L10", "D1", "R100", "U18"] `shouldBe` [Left' 10, Down 1, Right' 100, Up 18]
            it "should build intermediate values left" $
                DayThree.createIntermediateValues (Left' 3) (0,0) `shouldBe` [(-3,0), (-2, 0), (-1, 0), (0, 0)]
            it "should build intermediate values right" $
                DayThree.createIntermediateValues (Right' 3) (0,0) `shouldBe` [(3,0), (2, 0), (1, 0), (0, 0)]
            it "should build intermediate values down" $
                DayThree.createIntermediateValues (Down 3) (0,0) `shouldBe` [(0,-3), (0, -2), (0, -1), (0, 0)]
            it "should build intermediate values up" $
                DayThree.createIntermediateValues (Up 3) (0,0) `shouldBe` [(0,3), (0, 2), (0, 1), (0, 0)]
            it "should build do the whole list and come back normal" $
                DayThree.mapRoute [Up 3, Down 3] `shouldBe` [(0, 0), (0, 1), (0, 2), (0, 3), (0, 2), (0, 1), (0,0)]
            it "should do manhattan distance" $
                DayThree.manhattanDistance (-2, 3) `shouldBe` 5
            it "sohuld count steps" $
                DayThree.countSteps [(1,1), (5,5)] [(0,0), (0,80), (1,1), (80, 0), (5, 5)] `shouldBe` [2, 4]
        describe "day three tests" $ do
            it "should pass the first test" $
                DayThree.findShortestPoint "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83" `shouldBe` 159
            it "should pass the second test" $
                DayThree.findShortestPoint "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" `shouldBe` 135
        describe "day three part two tests" $ do
            it "should pass the base case" $ do
                DayThree.findShortestSteps "R8,U5,L5,D3" "U7,R6,D4,L4" `shouldBe` 30
            it "should pass the first test" $
                DayThree.findShortestSteps "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83" `shouldBe` 610
            it "should pass the second test" $
                DayThree.findShortestSteps "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" `shouldBe` 410

