import qualified Data.Set as Set
import Text.ParserCombinators.ReadP
import Control.Applicative ((<*), liftA2)
import Control.Monad (forM_)

type Coord = (Int, Int)

data Fold = FoldX Int | FoldY Int deriving Show

parseFile :: ReadP (Set.Set Coord, [Fold])
parseFile = do
    coords <- manyTill parseCoord (char '\n')
    folds <- manyTill parseFold eof
    pure (Set.fromList coords, folds)
parseCoord :: ReadP Coord
parseCoord = liftA2 (,) (readS_to_P reads <* char ',') (readS_to_P reads <* char '\n')
parseFold :: ReadP Fold
parseFold = choice
    [ FoldX <$> (string "fold along x=" >> readS_to_P reads <* char '\n')
    , FoldY <$> (string "fold along y=" >> readS_to_P reads <* char '\n') ]

printCoords :: Set.Set Coord -> IO ()
printCoords coords = do
    let maxX = Set.foldr max 0 $ Set.map fst coords
        maxY = Set.foldr max 0 $ Set.map snd coords
    forM_ [0..maxY] $ \y -> do
        forM_ [0..maxX] $ \x -> do
            putStr $ if Set.member (x,y) coords then "#" else "."
        putStrLn ""

fold :: Fold -> Set.Set Coord -> Set.Set Coord
fold (FoldX ax) = Set.map $ \(x, y) -> (if x > ax then 2*ax - x else x, y)
fold (FoldY ay) = Set.map $ \(x, y) -> (x, if y > ay then 2*ay - y else y)

folds :: Set.Set Coord -> [Fold] -> Set.Set Coord
folds = foldl (flip fold)

main :: IO ()
main = do
    putStrLn "testInput:"
    [((testCoords, testFolds), "")] <- readP_to_S parseFile <$> readFile "testInput"
    print $ Set.size $ folds testCoords [head testFolds]
    printCoords $ folds testCoords testFolds

    putStrLn "input:"
    [((coords, fs), "")] <- readP_to_S parseFile <$> readFile "input"
    printCoords $ folds coords fs