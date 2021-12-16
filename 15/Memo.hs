import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as Map
import Control.Monad (forM, forM_)
import Control.Monad.State.Strict
import Data.Char (isDigit, digitToInt, chr)
import Data.Maybe (catMaybes)

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

data Traversal = Traversal { accRisk :: Int, path :: [Coord] } deriving (Show, Eq)

printTraversal :: Map -> Traversal -> IO ()
printTraversal (Map c w h) (Traversal ar t) = do
    forM_ [0..h] $ \y -> do
        forM_ [0..w] $ \x -> 
            let stepped = (x,y) `elem` t
                risk = show (c Map.! (x,y))
            in putStr $ if stepped then yellow risk else risk
        putStrLn ""
    putStrLn $ "Total risk: " ++ show ar
    where
        yellow s = [chr 27] ++ "[33m" ++ s ++ [chr 27] ++ "[m"

type MemoS = State (Map.Map Coord (Maybe Traversal)) (Maybe Traversal)

leastRiskFrom :: Map -> Coord -> MemoS
leastRiskFrom m@(Map cave w h) (x,y) = lookUpdate (x, y) $ do
    if x > w || y > h then
        pure Nothing
    else do
        right <- leastRiskFrom m (x+1, y)
        down <- leastRiskFrom m (x, y+1)
        case catMaybes [right, down] of
            [] -> if x == w && y == h then pure (Just (Traversal (cave Map.! (x,y)) [(x,y)] )) else error "This error?"
            [Traversal ar path] -> pure . Just $ Traversal (ar + cave Map.! (x,y)) ((x,y):path)
            [Traversal ar path, Traversal br pbth] ->
                pure . Just $ if ar < br then Traversal (ar + cave Map.! (x,y)) ((x,y):path)
                                         else Traversal (br + cave Map.! (x,y)) ((x,y):pbth)
            _ -> error "Never happens"
        
lookUpdate :: Coord -> MemoS -> MemoS
lookUpdate key f = do
  memo <- gets (Map.lookup key)
  case memo of
    Just v -> return v
    Nothing -> do
      v <- f
      modify $ Map.insert key v
      return v

solveMemo :: Map -> Maybe Int
solveMemo m@(Map cave w h) =
    let startRisk = cave Map.! (0,0)
        result = accRisk <$> evalState (leastRiskFrom m (0,0)) Map.empty
    in subtract startRisk <$> result

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

main :: IO ()
main = do
    [(mTest, "")] <- readP_to_S parseFile <$> readFile "testInput"
    putStrLn $ "Solving testInput: " ++ show (solveMemo mTest)
    let mExpanded = expand5x5 mTest
    [(expected, "")] <- readP_to_S parseFile <$> readFile "testExpansion"
    putStrLn $ "Expanded map matches expected: " ++ show (mExpanded == expected)

    putStrLn $ "Sovling expanded testInput: " ++ show (solveMemo (expand5x5 mTest))

    [(m, "")] <- readP_to_S parseFile <$> readFile "input"
    putStrLn $ "Solving input: " ++ show (solveMemo m)
    putStrLn $ "Solving expanded input: " ++ show (solveMemo (expand5x5 m))
 