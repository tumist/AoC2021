-- This code uses the dijkstra algorithm from search-algorithms-0.3.1
-- Otherwise it's copy-paste from Memo.hs
import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as Map
import Control.Monad (forM)
import Data.Char (isDigit, digitToInt)
import Data.Maybe (catMaybes)
import Algorithm.Search (dijkstra)

type Coord = (Int, Int)
x, y :: Coord -> Int
x = fst
y = snd
data Map = Map { cave :: Map.Map Coord Int
               , width :: Int
               , height :: Int } deriving (Show, Eq)

parseFile :: ReadP Map
parseFile = do
    lns <- manyTill parseLine eof
    lst <- forM (zip [0..] lns) $ \(y, line) ->
        forM (zip [0..] line) $ \(x, digit) -> pure ((x,y), digit)
    let dimension = fst . last . last $ lst
    pure $ Map (Map.fromList (concat lst)) (x dimension) (y dimension)
    where
        parseLine :: ReadP [Int]
        parseLine = do
            digits <- map digitToInt <$> munch1 isDigit
            char '\n'
            pure digits

mapExpansion :: Map -> Coord -> Int
mapExpansion m@(Map cave w h) (x,y)
  | x <= w && y <= h = cave Map.! (x, y)
  | otherwise =
    let (tilex, modx) = x `divMod` (w+1)
        (tiley, mody) = y `divMod` (h+1)
        sum = mapExpansion m (if x > w then (x - w - 1, y) else (x, y-h-1)) + 1
    in if sum > 9 then 1 else sum

expand5x5 :: Map -> Map
expand5x5 m@(Map c w h) =
    let c' = Map.fromList [((x,y), mapExpansion m (x,y)) | x <- [0..5*(w+1)-1], y <- [0..5*(h+1)-1]]
        maxX = maximum . map x $ Map.keys c'
        maxY = maximum . map y $ Map.keys c'
    in Map c' maxX maxY

solveDijkstra m@(Map cave w h) = fst <$> 
    dijkstra nextStates (\from to -> cave Map.! to) (\(x,y) -> x == w && y == h) (0,0)
    where
        nextStates :: Coord -> [Coord]
        nextStates (x,y) = filter (`Map.member` cave) [(x+1, y), (x, y+1)]

main :: IO ()
main = do
    [(mTest, "")] <- readP_to_S parseFile <$> readFile "testInput"
    putStrLn $ "Solving testInput: " ++ show (solveDijkstra mTest)
    let mExpanded = expand5x5 mTest
    [(expected, "")] <- readP_to_S parseFile <$> readFile "testExpansion"
    putStrLn $ "Expanded map matches expected: " ++ show (mExpanded == expected)

    putStrLn $ "Sovling expanded testInput: " ++ show (solveDijkstra (expand5x5 mTest))

    [(m, "")] <- readP_to_S parseFile <$> readFile "input"
    putStrLn $ "Solving input: " ++ show (solveDijkstra m)
    putStrLn $ "Solving expanded input: " ++ show (solveDijkstra (expand5x5 m))
