import Test.Hspec
import DayOne as DayOne
import DayTwo as DayTwo

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
