module DayThree where

import Data.List
import Data.List.Split
import Data.List.Index
import qualified Data.Set as Set


data Move = Up Int
          | Down Int
          | Left' Int
          | Right' Int
          deriving (Show, Eq)

parseMoves :: [String] -> [Move]
parseMoves inputMoves =
    map (\(dir : rest) ->
            let
                value = read rest :: Int
            in
                case dir of
                  'U' ->
                      Up value
                  'D' ->
                      Down value
                  'R' ->
                      Right' value
                  'L' ->
                      Left' value
                  _ ->
                      error "unknown direction") inputMoves

createIntermediateValues :: Move -> (Int, Int) -> [(Int, Int)]
createIntermediateValues move start =
                            let
                                (x, y) = start
                            in
                              case move of
                                    Up amount ->
                                      reverse $ map (\i -> (x, i)) [y..(y + amount)]
                                    Down amount ->
                                      map (\i -> (x, i)) [(y - amount)..y]
                                    Left' amount ->
                                      map (\i -> (i, y)) [(x - amount)..x]
                                    Right' amount ->
                                      reverse $ map (\i -> (i, y)) [x..(x + amount)]

mapRoute :: [Move] -> [(Int, Int)]
mapRoute moves =
        mapRouteHelper (0, 0) moves

mapRouteHelper :: (Int, Int) -> [Move] -> [(Int, Int)]
mapRouteHelper start moves =
    reverse $ foldl (\(prev : rest) move ->
                let
                    newCoords =
                            createIntermediateValues move prev
                in
                    newCoords ++ rest
                ) [start] moves

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x, y) =
    abs (0 - x) + abs (0 - y)

findShortestSteps' :: [Move] -> [Move] -> [Int]
findShortestSteps' one two =
    let
        oneRoute = mapRoute one
        setOneRoute = Set.fromList oneRoute
        twoRoute = mapRoute two
        setTwoRoute = Set.fromList twoRoute
        common = Set.intersection setOneRoute setTwoRoute
        commonList = filter (/= (0,0)) $ Set.toList common
        oneSteps = countSteps commonList oneRoute
        twoSteps = countSteps commonList twoRoute
        comboSteps = zipWith (+) oneSteps twoSteps
    in
        comboSteps

findShortestSteps :: String -> String -> Int
findShortestSteps first second =
    let
        firstMoves = parseMoves $ splitOn "," first
        secondMoves = parseMoves $ splitOn "," second
    in
        head $ sort $ findShortestSteps' firstMoves secondMoves

countSteps :: [(Int, Int)] -> [(Int, Int)] -> [Int]
countSteps commoncoords route =
    let
        indexList = indexed route
    in
        map (counter indexList) commoncoords

counter :: [(Int, (Int, Int))] -> (Int, Int) -> Int
counter [] _ = 0
counter ((i, x):xs) match =
    if x == match then
        i
    else
      counter xs match

findShortestPoint' :: [Move] -> [Move] -> Int
findShortestPoint' one two =
    let
        oneRoute = Set.fromList $ mapRoute one
        twoRoute = Set.fromList $ mapRoute two
        common = Set.intersection oneRoute twoRoute
        commonList = Set.toList common
    in
        head $ filter (/= 0) $ sort $ map manhattanDistance commonList

findShortestPoint :: String -> String -> Int
findShortestPoint first second =
    let
        firstMoves = parseMoves $ splitOn "," first
        secondMoves = parseMoves $ splitOn "," second
    in
        findShortestPoint' firstMoves secondMoves
