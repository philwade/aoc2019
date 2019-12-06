module DayOne where

import Data.List as List

getFuel :: Int -> Int
getFuel input =
    (input `div` 3) - 2

runAll :: [Int] -> Int
runAll numbers =
    List.sum $ List.map getFuel numbers

getDoubleFuel :: Int -> Int
getDoubleFuel input =
   let
        thisFuel = getFuel input
    in
        if thisFuel > 0 then
            thisFuel + getDoubleFuel thisFuel
        else
            0

sumDouble :: [Int] -> Int
sumDouble numbers =
    List.sum $ List.map getDoubleFuel numbers
