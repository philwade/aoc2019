module DayTwo where

returnIndex :: Int -> [Int] -> Int
returnIndex index list = list !! index

runProgram :: [Int] -> [Int]
runProgram inputs =
    runHelper (0, inputs)

runHelper :: (Int, [Int]) -> [Int]
runHelper (offset, wholeList) =
    let
        opt : rest = drop (offset * 4) wholeList
    in
        case opt of
            99 -> wholeList
            1 ->
                let
                    i1 : i2 : position : _  = rest
                    v1 = returnIndex i1 wholeList
                    v2 = returnIndex i2 wholeList
                    newV = v1 + v2
                    newList = setIndex position newV wholeList
                in
                    runHelper (offset + 1, newList)
            2 ->
                let
                    i1 : i2 : position : _  = rest
                    v1 = returnIndex i1 wholeList
                    v2 = returnIndex i2 wholeList
                    newV = v1 * v2
                    newList = setIndex position newV wholeList
                in
                    runHelper (offset + 1, newList)
            _ -> error "Terrubke optcode"

setIndex :: Int -> Int -> [Int] -> [Int]
setIndex i v list =
    let
        indexed = zip [0..] list
        updated = map (\(index, value) ->
                        if index == i then
                            v
                        else
                            value) indexed
    in
      updated

bruteForce :: [Int] -> IO ()
bruteForce input =
    mapM_ (\verb ->
                do
                    mapM_ (\noun ->
                            do
                                let outPut = verbNounInput input verb noun
                                if outPut == 19690720 then
                                    do
                                      putStrLn "verb:"
                                      putStrLn $ show verb
                                      putStrLn "noun:"
                                      putStrLn $ show noun
                                else
                                    putStr ""
                                  ) [0..99] ) [0..99]

verbNounInput :: [Int] -> Int -> Int -> Int
verbNounInput list verb noun =
        let
            newP = setIndex 2 verb $ DayTwo.setIndex 1 noun list
            finished = runProgram newP
        in
            returnIndex 0 finished
