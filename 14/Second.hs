import qualified Data.Map.Strict as Map
import Data.Map.Merge.Strict (merge, preserveMissing, zipWithMatched)
import Text.ParserCombinators.ReadP hiding (get)
import Data.Char (isAlpha)
import Control.Monad.State.Strict

type Elem = Char
type Pair = (Elem, Elem)
type Polymerization = Map.Map Pair Elem
type ElemCount = Map.Map Elem Int

parseFile :: ReadP (String, Polymerization)
parseFile = do
    template <- munch1 isAlpha
    char '\n'
    char '\n'
    rules <- manyTill parseRule eof
    pure (template, Map.fromList rules)
    where
        parseRule = do
            a <- satisfy isAlpha
            b <- satisfy isAlpha
            string " -> "
            to <- satisfy isAlpha
            char '\n'
            pure ((a,b), to)

-- pairs NNCB = NN, NC, CB
pairs :: String -> [Pair]
pairs s = zip s (tail s)

-- Merge Maps with addition on values
mergePlus :: ElemCount -> ElemCount -> ElemCount
mergePlus = merge preserveMissing preserveMissing (zipWithMatched (const (+)))

-- Memoization approach:
-- The memo is a Map with key (Height, Pair) and it's value the counts of Elements
type MemoKey = (Int, Pair)
type MemoS = State (Map.Map MemoKey ElemCount) ElemCount

-- countGen counts only generated Elements for a Pair at a given height
-- N       N
-- N   C   N countGen 1 NN = C
-- N B C C N countGen 2 NN = BCC
-- NBBBCNCCN countGen 3 NN = BBBCNCC
countGenMemo :: Polymerization -> Int -> Pair -> MemoS
countGenMemo _ 0 (a,b) = pure Map.empty
countGenMemo z h (a,b) =
    let key = (h, (a,b))
    in lookUpdate key $ do
        let g = z Map.! (a,b)
        left  <- countGenMemo z (h-1) (a,g)
        right <- countGenMemo z (h-1) (g,b)
        pure $ Map.fromList [(g, 1)] `mergePlus` left `mergePlus` right

lookUpdate :: (Int, Pair) -> MemoS -> MemoS
lookUpdate key f = do
  memo <- gets (Map.lookup key)
  case memo of
    Just v -> return v
    Nothing -> do
      v <- f
      modify $ Map.insert key v
      return v

countElementsMemo :: Polymerization -> Int -> String -> MemoS
countElementsMemo z h s = do
    -- Elements in the polymer template are not counted by countGenMemo
    let notGen = Map.fromListWith (+) (map (\c -> (c, 1)) s)
    -- Run countGenMemo on every pair in polymer template
    v <- forM (pairs s) (countGenMemo z h)
    pure $ foldl1 mergePlus (notGen : v)

solve :: Map.Map Pair Elem -> Int -> [Char] -> Int
solve z h s =
    let counts = evalState (countElementsMemo z h s) Map.empty
        countMax = maximum (Map.elems counts)
        countMin = minimum (Map.elems counts)
    in countMax - countMin

main :: IO ()
main = do
    [((ts, tz), "")] <- readP_to_S parseFile <$> readFile "testInput"
    [((s, z), "")] <- readP_to_S parseFile <$> readFile "input"
    
    print $ solve tz 40 ts
    print $ solve z 40 s