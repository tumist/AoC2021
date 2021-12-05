{-# LANGUAGE TupleSections #-}
import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as Map
import Control.Monad (forM_)
import Data.List (sort)

type Point = (Int, Int)
x :: (x, y) -> x
x = fst
y :: (x, y) -> y
y = snd
type Line = (Point, Point)

-- Get every Point of a Line
linePoints :: Line -> [Point]
linePoints l = linePoints' [startPoint] endPoint (dx, dy)
  where
    startPoint = fst l
    endPoint = snd l
    dx = x endPoint `direction` x startPoint
    dy = y endPoint `direction` y startPoint
    direction a b
      | a > b = 1
      | a < b = -1
      | otherwise = 0
linePoints' :: [Point] -> Point -> Point -> [Point]
linePoints' [] _ _ = error "This doesn't happen if called through linePoints"
linePoints' pts@(p1:rest) dest (dx, dy)
  | p1 == dest = pts
  | otherwise = linePoints' ((x p1 + dx, y p1 + dy):pts) dest (dx, dy)

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
-- Sorting the line points and using fromAscListWith is considerably
-- faster than using fromListwith
mkMap :: [Line] -> Map.Map Point Int
mkMap = Map.fromAscListWith (+) . map (, 1) . sort . concatMap linePoints

solve :: [Line] -> Int
solve = Map.size . Map.filter (> 1) . mkMap

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
