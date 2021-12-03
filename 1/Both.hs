-- Both parts of Day 1
-- A quick way to compare each element in a list with the next
-- is to use `zip list (tail list)`.
--  list                 [1,     2,     3,     4,    5]
--  tail list            [2,     3,     4,     5]
--  zip list (tail list) [(1,2), (2,3), (3,4), (4,5)]
-- The first part then compares the tuples using > and counts the Trues.
solve1 :: [Integer] -> Int
solve1 list@(_:tail) = length $ filter (== True) $ map (\(a,b) -> b > a) $ zip list tail
-- Good luck today as there were no edge cases to care about!

-- The second part reuses `solve1` by passing it a list where all thee consecutive
-- numbers are summed using `zip3`.
-- The argument unpacking `list@(_:tail@(_:ttail))` is ugly, but short way to get the tail
-- of the list as well as the tail of the tail.
solve2 :: [Integer] -> Int
solve2 list@(_:tail@(_:ttail)) = solve1 $ map (\(a,b,c) -> a+b+c) $ zip3 list tail ttail

parse :: FilePath -> IO [Integer]
parse fp = do
    f <- readFile fp
    return $ map read (lines f)

main :: IO ()
main = do
    parse "testInput" >>= print . solve1
    parse "input" >>= print . solve1

    parse "testInput" >>= print . solve2
    parse "input" >>= print . solve2