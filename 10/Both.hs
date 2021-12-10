{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Text.ParserCombinators.ReadP
import Data.Maybe (catMaybes)
import Data.List (sort)

parseLine :: ReadP (Maybe Char)
parseLine = do
    open <- satisfy (`elem` "([{<")
    mIllegals <- many parseLine
    -- This parser returns at the first illegal character
    case catMaybes mIllegals of
        firstIllegal:_ -> pure $ Just firstIllegal
        [] -> do
            close <- satisfy (`elem` ")]}>")
            pure $ if close == closing open then Nothing else Just close

closing :: Char -> Char
closing '(' = ')'
closing '[' = ']'
closing '{' = '}'
closing '<' = '>'

score1 :: Char -> Int
score1 ')' = 3
score1 ']' = 57
score1 '}' = 1197
score1 '>' = 25137

lineScore :: String -> Int
lineScore s =
    case readP_to_S parseLine s of
        [] -> 0
        (Nothing, _):_ -> 0
        (Just illegal, _):_ -> score1 illegal

solve1 :: [String] -> Int
solve1 = sum . map lineScore

-- Part 2

-- This parser will complete a chunk
-- If input is two cunks like "()<", it will parse ()
-- but leave "<" as rest. Some if the input is like that.
completeChunk :: ReadP String
completeChunk = do
    open <- satisfy (`elem` "([{<")
    completions <- concat <$> many completeChunk
    rest <- look
    let isEof = rest == ""
    if isEof then
        pure $ completions ++ [closing open]
    else do
        close <- satisfy (`elem` ")]}>")
        pure completions

-- Here we deal with the multiple chunks:
-- if the parser completes a (complete) chunk, run it again on the rest
complete :: String -> String
complete s =
    case readP_to_S completeChunk s of
        ("", rest):_ -> complete rest
        (completion, ""):_ -> completion

score2 :: Char -> Int
score2 ')' = 1
score2 ']' = 2
score2 '}' = 3
score2 '>' = 4

completionScore :: String -> Int
completionScore = foldl (\acc chr -> acc * 5 + score2 chr) 0

-- The parser from part 1 can be used to determine if an input is corrupt
isCorrupt :: String -> Bool
isCorrupt s =
    case readP_to_S parseLine s of
        [] -> False
        (Nothing, _):_ -> False
        _ -> True

solve2 :: [String] -> Int
solve2 ss =
    let notCorrupted = filter (not . isCorrupt) ss
        completions = map complete notCorrupted
        scores = map completionScore completions
        middleScore = sort scores !! (length scores `div` 2)
    in middleScore

main :: IO ()
main = do
    testInput <- lines <$> readFile "testInput"
    input <- lines <$> readFile "input"
    print $ solve1 testInput
    print $ solve1 input
    print $ solve2 testInput
    print $ solve2 input
