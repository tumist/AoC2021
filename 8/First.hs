import Text.ParserCombinators.ReadP
import qualified Data.Set as Set

parseFile :: ReadP [([String], [String])]
parseFile = manyTill parseLine eof
parseLine = do
    sp <- munch1 (\c -> 'a' <= c && c <= 'g') `sepBy` char ' '
    string " | "
    ov <- munch1 (\c -> 'a' <= c && c <= 'g') `sepBy` char ' '
    char '\n'
    pure (sp, ov)


-- The digits which have unique number of segments for part 1
digitToSegments :: Int -> Int
digitToSegments 1 = 2
digitToSegments 4 = 4
digitToSegments 7 = 3
digitToSegments 8 = 7
digitToSegments _ = error "Not needed for part 1"

solve1 :: [[String]] -> Int
solve1 = length . concat
       . map (filter (`elem` map digitToSegments [1,4,7,8]))
       . map (map length)

main :: IO ()
main = do
  [(testInput, "")] <- readP_to_S parseFile <$> readFile "testInput"
  [(input, "")] <- readP_to_S parseFile <$> readFile "input"
  print $ solve1 $ map snd testInput
  print $ solve1 $ map snd input