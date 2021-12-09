import Text.ParserCombinators.ReadP

import Data.Char (isDigit, digitToInt)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Control.Monad (forM)
import Data.List (sort)

type Coord = (Int, Int)
x, y :: Coord -> Int
x = fst
y = snd
type Height = Int
type HeightMap = Map.Map Coord Int

parseFile :: ReadP HeightMap
parseFile = do
    lns <- manyTill parseLine eof
    lst <- forM (zip [0..] lns) $ \(y, line) ->
        forM (zip [0..] line) $ \(x, digit) -> pure ((x,y), digit)
    pure $ Map.fromList (concat lst)
    where
        parseLine :: ReadP [Int]
        parseLine = do
            digits <- munch1 isDigit
            char '\n'
            pure $ map digitToInt digits

adjacentHV = [(-1,0), (0,-1), (1,0),(0,1)]

isLowpoint :: HeightMap -> Coord -> Height -> Bool
isLowpoint hm coord h =
    let adjacent = [Map.lookup (x coord + dx, y coord + dy) hm | (dx, dy) <- adjacentHV]
    in all (h <) (catMaybes adjacent)

lowPoints :: HeightMap -> HeightMap
lowPoints hm = Map.filterWithKey (isLowpoint hm) hm

solve1 :: HeightMap -> Int
solve1 = sum . map (+1) . map snd . Map.toList . lowPoints

-- Part 2
type Basin = Set.Set Coord

adjacents :: Coord -> [Coord]
adjacents (x,y) = map (\(dx,dy) -> (x+dx,y+dy)) adjacentHV

basin :: HeightMap -> Coord -> Basin
basin hm startPoint = basin' hm (Set.fromList [startPoint])

setConcat :: Ord a => Set.Set [a] -> Set.Set a
setConcat = Set.unions . Set.toList .  Set.map Set.fromList

basin' :: HeightMap -> Set.Set Coord -> Set.Set Coord
basin' hm points =
    let explore = Set.difference (setConcat $ Set.map adjacents points) points
        expand = Set.filter inBasin explore
        inBasin :: Coord -> Bool
        inBasin coord =
            case Map.lookup coord hm of
                Nothing -> False
                Just i -> i < 9
        expanded = Set.union points expand
    in if expand == Set.empty then expanded else basin' hm expanded

basins :: HeightMap -> [Basin]
basins hm = map (basin hm) . map fst . Map.toList . lowPoints $ hm

basinSize :: Basin -> Int
basinSize = Set.size

solve2 :: HeightMap -> Int
solve2 = product . take 3 . reverse . sort . map basinSize . basins 

main :: IO ()
main = do
    [(test, "")] <- readP_to_S parseFile <$> readFile "testInput"
    [(input, "")] <- readP_to_S parseFile <$> readFile "input"
    print $ solve1 test
    print $ solve1 input
    print $ solve2 test
    print $ solve2 input
    return ()