import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as Map
import Control.Monad (forM_)

type Point = (Int, Int)
x :: (x, y) -> x
x = fst
y :: (x, y) -> y
y = snd
type Line = (Point, Point)

-- Get every Point of a Line
linePoints :: Line -> [Point]
linePoints l = linePoints' [fst l] (snd l)
linePoints' :: [Point] -> Point -> [Point]
linePoints' [] _ = error "This doesn't happen if called through linePoints"
linePoints' pts@(p1:rest) dest
  | p1 == dest = pts
  | otherwise = let dx = x dest `direction` x p1
                    dy = y dest `direction` y p1
                in linePoints' ((x p1 + dx, y p1 + dy):pts) dest
  where
    direction a b
      | a > b = 1
      | a < b = -1
      | otherwise = 0

-- Input parser
sepTuple :: ReadP a -> ReadP sep -> ReadP (a,a)
sepTuple a sep = a >>= \first -> sep >> a >>= \second -> pure (first,second)
parseLines :: ReadP [Line]
parseLines = manyTill parseLine eof
parseLine :: ReadP Line
parseLine = sepTuple parsePoint (string " -> ")
parsePoint :: ReadP Point
parsePoint = sepTuple readInt (char ',')
  where readInt = readS_to_P reads :: ReadP Int

-- Create a map counting every occurance of a Point
mkMap :: [Line] -> Map.Map Point Int
mkMap lines = 
  let points = concatMap linePoints lines
  in foldr (\key map -> Map.insertWith (+) key 1 map) Map.empty points

solve :: [Line] -> Int
solve lines =
  let map = mkMap lines
  in Map.size $ Map.filter (> 1) map

-- For debugging
printMap :: Map.Map Point Int -> IO ()
printMap m =
  forM_ [0..9] $ \y -> do
    forM_ [0..9] $ \x ->
      putStr (maybe "." show (Map.lookup (x, y) m))
    putStrLn ""

isHorizVerti :: Line -> Bool
isHorizVerti (p1, p2) = x p1 == x p2 || y p1 == y p2

main :: IO ()
main = do
  [(testLines, "")] <- readP_to_S parseLines <$> readFile "testInput"
  [(lines, "")] <- readP_to_S parseLines <$> readFile "input"

  print $ solve (filter isHorizVerti testLines)
  print $ solve (filter isHorizVerti lines)
  
  print $ solve testLines
  print $ solve lines
