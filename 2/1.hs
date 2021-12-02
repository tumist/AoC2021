-- The first part of Day 2
-- Each instruction can be encoded as a position delta:
--  forward 5 -> Pos 5 0
--  down 5    -> Pos 0 5
-- and then summed to reach a final Pos 5 5.
data Position = Pos Int -- Horizontal
                    Int -- Depth
                deriving Show

sumPos :: [Position] -> Position
sumPos = foldl plusPos (Pos 0 0)
  where
    plusPos (Pos h d) (Pos h' d') = Pos (h+h') (d+d')

mulPos (Pos h d) = h * d

parseFile :: FilePath -> IO [Position]
parseFile fp = do
    f <- fmap lines (readFile fp)
    return $ map (parseLine . words) f

-- No pretty parser today either!
parseLine :: [String] -> Position
parseLine ["forward", n] = Pos (read n) 0
parseLine ["down", n] = Pos 0 (read n)
parseLine ["up", n] = Pos 0 (negate $ read n )
parseLine e = error $ "parseLine: " ++ show e

main :: IO ()
main = do
    parseFile "testInput" >>= print . mulPos . sumPos
    parseFile "input" >>= print . mulPos . sumPos