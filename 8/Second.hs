-- This file is only commited as a demonstration how
-- uncomprehensible and badly designed haskell code *can* be.
-- However, it runs surprisingly fast
{-# LANGUAGE TupleSections #-}
import Text.ParserCombinators.ReadP
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (nub)
import Control.Monad (forM)

data Segment
    = Top
    | TopLeft
    | TopRight
    | Middle
    | BottomLeft
    | BottomRight
    | Bottom
    deriving (Show, Eq, Ord)

segments :: [Segment]
segments = [Top, TopLeft, TopRight, Middle, BottomLeft, BottomRight, Bottom]

newtype Digit = Digit (Map.Map Segment (Set.Set Char)) deriving Show

-- Digit with all Segments set to "abcdefg"
mkDigit :: Digit
mkDigit = Digit $ Map.fromList (map (, Set.fromList "abcdefg") segments)

digitSegments :: Int -> [Segment]
digitSegments 0 = [Top, TopLeft, TopRight, BottomLeft, BottomRight, Bottom]
digitSegments 1 = [TopRight, BottomRight]
digitSegments 2 = [Top, TopRight, Middle, BottomLeft, Bottom]
digitSegments 3 = [Top, TopRight, Middle, BottomRight, Bottom]
digitSegments 4 = [TopLeft, TopRight, Middle, BottomRight]
digitSegments 5 = [Top, TopLeft, Middle, BottomRight, Bottom]
digitSegments 6 = [Top, TopLeft, Middle, BottomLeft, BottomRight, Bottom]
digitSegments 7 = [Top, TopRight, BottomRight]
digitSegments 8 = [Top, TopLeft, TopRight, Middle, BottomLeft, BottomRight, Bottom]
digitSegments 9 = [Top, TopLeft, TopRight, Middle, BottomRight, Bottom]
digitSegments _ = error "Not single digit"

segmentsDigit :: Set.Set Segment -> Int
segmentsDigit s
  | s == Set.fromList [Top, TopLeft, TopRight, BottomLeft, BottomRight, Bottom] = 0
  | s == Set.fromList [TopRight, BottomRight] = 1
  | s == Set.fromList [Top, TopRight, Middle, BottomLeft, Bottom] = 2
  | s == Set.fromList [Top, TopRight, Middle, BottomRight, Bottom] = 3
  | s == Set.fromList [TopLeft, TopRight, Middle, BottomRight] = 4
  | s == Set.fromList [Top, TopLeft, Middle, BottomRight, Bottom] = 5
  | s == Set.fromList [Top, TopLeft, Middle, BottomLeft, BottomRight, Bottom] = 6
  | s == Set.fromList [Top, TopRight, BottomRight] = 7
  | s == Set.fromList [Top, TopLeft, TopRight, Middle, BottomLeft, BottomRight, Bottom] = 8
  | s == Set.fromList [Top, TopLeft, TopRight, Middle, BottomRight, Bottom] = 9
  | otherwise = error "Segments don't make a digit"

-- For debugging
showDigit :: Digit -> IO ()
showDigit d = do
    putStrLn $ " " ++ [s d Top, s d Top]
    putStrLn $ [s d TopLeft] ++ "  " ++ [s d TopRight]
    putStrLn $ " " ++ [s d Middle, s d Middle]
    putStrLn $ [s d BottomLeft] ++ "  " ++ [s d BottomRight]
    putStrLn $ " " ++ [s d Bottom, s d Bottom]
    where
        s :: Digit -> Segment -> Char
        s (Digit m) seg = case Map.lookup seg m of
                            Just s -> let [c] = Set.toList s in c
                            Nothing -> ' '

mkNumDigit n = Digit $ Map.fromList $ zip (digitSegments n) (repeat $ Set.fromList "x")
  ++ zip (invertSegments $ digitSegments n) (repeat $ Set.empty)

digitSegments' :: Int -> Set.Set Segment
digitSegments' n = Set.fromList (digitSegments n)

-- unions when more than one set of segments match the signalPattern length
signalPatternSegments :: String -> [Segment]
signalPatternSegments s = nub . concat $ filter ((==) (length s) . length) $ map digitSegments [0..9]

-- intersections
signalPatternSegments' :: String -> Set.Set Segment
signalPatternSegments' s = foldl1 Set.intersection $ map Set.fromList $ filter ((==) (length s) . length) $ map digitSegments [0..9]

invertSegments :: [Segment] -> [Segment]
invertSegments s = filter (`notElem` s) segments

-- Given Digit and signal pattern "ab"
-- we set signalPatternSegments to intersect with "ab"
-- and invertSegments have "ab" removed
applySignal :: String -> Digit -> Digit
applySignal signal digit =
  let segIntersect = Set.toList $ signalPatternSegments' signal
      segRemove    = invertSegments (signalPatternSegments signal)
      sigSet       = Set.fromList signal

      intersected  = foldr (\seg (Digit m) -> Digit (Map.adjust (`Set.intersection` sigSet) seg m)) digit segIntersect
      removed = foldr (\seg (Digit m) -> Digit (Map.adjust (`Set.difference` sigSet) seg m)) intersected segRemove
      -- This doesn't reduce every segment down to one letter, but easily solvable
      Digit map = removed
      ones = Set.unions $ Map.elems $ Map.filter (\s -> Set.size s == 1 ) map
  in Digit $ Map.map (\a -> if Set.size a > 1 then Set.difference a ones else a) map

applySignals :: [String] -> Digit
applySignals ss = foldl (flip applySignal) mkDigit ss

digitSignal :: Digit -> String -> Int
digitSignal (Digit map) str = segmentsDigit . Set.fromList $ Map.keys $ Map.filter (\v -> v /= Set.empty) $ Map.map (\s -> Set.intersection (Set.fromList str) s) map

digitsSignal :: Digit -> [String] -> Int
digitsSignal d ss = read . concat $ map (show . digitSignal d) ss

parseFile :: ReadP [([String], [String])]
parseFile = manyTill parseLine eof
parseLine = do
    sp <- munch1 (\c -> 'a' <= c && c <= 'g') `sepBy` char ' '
    string " | "
    ov <- munch1 (\c -> 'a' <= c && c <= 'g') `sepBy` char ' '
    char '\n'
    pure (sp, ov)

main :: IO ()
main = do
  [(testInput, "")] <- readP_to_S parseFile <$> readFile "testInput"
  v <- forM testInput $ \(signalPatterns, digits) -> do
    let d = applySignals signalPatterns
        v = digitsSignal d digits
    print v
    pure v
  putStrLn $ "Sum: " ++ show (sum v)

  [(input, "")] <- readP_to_S parseFile <$> readFile "input"
  v <- forM input $ \(signalPatterns, digits) -> do
    let d = applySignals signalPatterns
        v = digitsSignal d digits
    print v
    pure v
  putStrLn $ "Sum: " ++ show (sum v)
