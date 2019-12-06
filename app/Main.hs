module Main where

import Data.List.Split
import DayOne as DayOne
import DayTwo as DayTwo

main :: IO ()
main = dayTwo

dayOne :: IO ()
dayOne =
    do
        putStrLn "Day one"
        input <- readFile "./inputs/dayone"
        let fuelInputs = map (\x -> read x :: Int) $ lines input
        let fuelSum = DayOne.runAll fuelInputs
        putStrLn "part one:"
        putStrLn $ show fuelSum
        let doubleSum = DayOne.sumDouble fuelInputs
        putStrLn "part two:"
        putStrLn $ show doubleSum

dayTwo :: IO ()
dayTwo =
    do
        putStrLn "Day two"
        input <- readFile "./inputs/daytwo"
        putStrLn "part one"
        let program = map (\x -> read x :: Int) $ splitOn "," input
        let final = DayTwo.verbNounInput program 2 12
        putStrLn $ show final
        DayTwo.bruteForce program

