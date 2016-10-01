import Data.List

-- used for doing benchmarks
import Data.Time.Clock

-- determines the next number in the Collatz sequence
nextCollatzNumber :: Int -> Int
nextCollatzNumber n
    | n `mod` 2 == 0 = n `div` 2
    | otherwise = 3 * n + 1

-- supposedly an optimized version of it, but testing proves otherwise
collatzLength' :: Int -> Integer
collatzLength' = (map collatzLength [0..] !!)

collatzLength :: Int -> Integer
collatzLength n =
    case n of
        1 -> 1
        _ -> 1 + collatzLength (nextCollatzNumber n)

-- used to find the longest sequence by only comparing the second 
-- in the tuple
compareSnd :: (t1,Integer) -> (t1,Integer) -> Ordering
compareSnd (_, a) (_, b) = compare a b

main :: IO ()
main = do
    -- putStrLn "Answer is: "
    -- let numbers = [1..1000000]
    -- let zippy = zip numbers (map collatzLength numbers)
    -- let highest = maximumBy compareSnd zippy
    -- print highest

    -- The largest viable starting number.
    let maximumNumber = 1000000

    let numbers = [1..maximumNumber]

    putStrLn "Starting naive..."
    start <- getCurrentTime
    -- print $ collatzLength 1000000
    let zippy = zip numbers (map collatzLength numbers)
    let highest = maximumBy compareSnd zippy
    print highest
    end <- getCurrentTime
    let diffTime = diffUTCTime end start
    let secondsElapsed = fromRational (toRational diffTime)
    putStrLn "Execution time: "
    print secondsElapsed

    putStrLn "Starting optimized..."
    start2 <- getCurrentTime
    -- print $ collatzLength' 1000000
    let zippy2 = zip numbers (map collatzLength' numbers)
    let highest2 = maximumBy compareSnd zippy2
    print highest2
    end2 <- getCurrentTime
    let diffTime2 = diffUTCTime end2 start2
    let secondsElapsed2 = fromRational (toRational diffTime2)
    putStrLn "Execution time: "
    print secondsElapsed2