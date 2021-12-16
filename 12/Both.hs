import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isAsciiLower)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

type Cave = String
type Caves = Map.Map Cave [Cave]

parseFile :: ReadP Caves
parseFile = do
    edges <- manyTill parseLine eof
    -- For an edge A-B, create both A->B and B->A
    -- The second part (value of the map) is a list as they'll be
    -- concatenated on duplicate keys
    let lst = concatMap (\(a,b) -> [(a,[b]), (b,[a])]) edges
    pure $ Map.fromListWith (++) lst

parseLine :: ReadP (Cave, Cave)
parseLine = do
    a <- munch1 isAlpha
    char '-'
    b <- munch1 isAlpha
    char '\n'
    pure (a, b)

isSmall :: Cave -> Bool
isSmall = isAsciiLower . head


data Path = Path { path :: [Cave]
                 , visitedSmall :: Set.Set Cave
                 , visitedSmallTwice :: Bool } deriving Show

startPath :: Path
startPath = Path ["start"] Set.empty False

-- Rule for part 1
-- Given current Path, may Santa go to Cave?
smallCaveOnce :: Path -> Cave -> Maybe Path
smallCaveOnce _ "start" = Nothing
smallCaveOnce p@(Path path visitedSmall _) edge =
    if isSmall edge && edge `Set.member` visitedSmall
    then Nothing
    else Just $ p { path = edge:path, visitedSmall = Set.insert edge visitedSmall }

oneSmallTwice :: Path -> Cave -> Maybe Path
oneSmallTwice _ "start" = Nothing
oneSmallTwice p@(Path path visitedSmall visitedSmallTwice) edge =
    if isSmall edge && edge `Set.member` visitedSmall
    then
        if not visitedSmallTwice
        then Just $ p { path = edge:path, visitedSmallTwice = True }
        else Nothing
    else Just $ p { path = edge:path, visitedSmall = Set.insert edge visitedSmall }

mkPaths :: Caves -> Cave -> (Path -> Cave -> Maybe Path) -> Path -> [Path]
mkPaths _ _ _ (Path [] _ _) = error "Give a start Cave to Path"
mkPaths m end rule pth@(Path p@(current:_) _ _) =
    if current == end then [pth]
    else
        let nexts = mapMaybe (rule pth) $ m ! current
        in concatMap (mkPaths m end rule) nexts

solve1 :: Caves -> Int
solve1 mp = length $ mkPaths mp "end" smallCaveOnce startPath

solve2 :: Caves -> Int
solve2 mp = length $ mkPaths mp "end" oneSmallTwice startPath

main = do
    [(test1, "")] <- readP_to_S parseFile <$> readFile "testInput1"
    [(test2, "")] <- readP_to_S parseFile <$> readFile "testInput2"
    [(test3, "")] <- readP_to_S parseFile <$> readFile "testInput3"
    [(input, "")] <- readP_to_S parseFile <$> readFile "input"
    print $ solve1 test1
    print $ solve1 test2
    print $ solve1 test3
    print $ solve1 input

    print $ solve2 test1
    print $ solve2 input
    