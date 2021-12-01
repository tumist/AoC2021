parse :: FilePath -> IO [Integer]
parse fp = do
    f <- readFile fp
    return $ map read (lines f)

solve1 :: [Integer] -> Int
solve1 list@(_:tail) = length $ filter (== True) $ map (\(a,b) -> b > a) $ zip list tail

solve2 :: [Integer] -> Int
solve2 list@(_:tail@(_:ttail)) = solve1 $ map (\(a,b,c) -> a+b+c) $ zip3 list tail ttail

main :: IO ()
main = do
    parse "testInput" >>= print . solve1
    parse "input" >>= print . solve1

    parse "testInput" >>= print . solve2
    parse "input" >>= print . solve2