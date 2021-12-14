import qualified Data.Map.Strict as Map
import Text.ParserCombinators.ReadP
import Data.Char (isAlpha)

parseFile :: ReadP (String, Map.Map String Char)
parseFile = do
    template <- munch1 isAlpha
    char '\n'
    char '\n'
    rules <- manyTill parseRule eof
    pure (template, Map.fromList rules)
    where
        parseRule = do
            from <- munch1 isAlpha
            string " -> "
            to <- satisfy isAlpha
            char '\n'
            pure (from, to)

polymerize :: Map.Map String Char -> String -> String
polymerize mp [a, b] = a : mp Map.! [a, b] : [b]
polymerize mp (a:b:rest) = a : mp Map.! [a, b] : polymerize mp (b:rest)

times :: Map.Map String Char -> String -> Int -> String
times mp s n = iterate (polymerize mp) s !! n

solve1 :: Map.Map String Char -> [Char] -> Int
solve1 mp s =
    let polymerized = times mp s 10
        countMap = Map.fromListWith (+) (map (\c -> (c, 1)) polymerized)
        countMax = maximum (Map.elems countMap)
        countMin = minimum (Map.elems countMap)
    in countMax - countMin

-- Takes way too long, see Second.hs for a memoized approach
solve2 :: Map.Map String Char -> [Char] -> Int
solve2 mp s =
    let polymerized = times mp s 40
        countMap = Map.fromListWith (+) (map (\c -> (c, 1)) polymerized)
        countMax = maximum (Map.elems countMap)
        countMin = minimum (Map.elems countMap)
    in countMax - countMin

main :: IO ()
main = do
    [((testStr, testRules), "")] <- readP_to_S parseFile <$> readFile "testInput"
    [((str, rules), "")] <- readP_to_S parseFile <$> readFile "input"
    print $ solve1 testRules testStr
    print $ solve1 rules str