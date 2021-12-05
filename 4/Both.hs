import Text.ParserCombinators.ReadP
import Data.List (transpose, inits, sortOn)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)

type Row = Set.Set Int
type Board = [Row]

-- Parser
-- Needs \n at the end of input (or rather, at the end of last bingo row)
parseBingo :: ReadP ([Int], [Board])
parseBingo = do
  draws <- readS_to_P reads `sepBy1` char ','
  char '\n'
  boards <- manyTill (char '\n' >> parseBoard) eof
  pure (draws, boards)
parseBoard :: ReadP Board
parseBoard = do
  rows <- count 5 $ parseRow >>= \row -> char '\n' >> pure row
  pure $ map Set.fromList (rows ++ transpose rows)
parseRow :: ReadP [Int]
parseRow = count 5 $ skipSpaces >> readS_to_P reads

-- All numbers on a Board
boardNumbers :: Board -> Set.Set Int
boardNumbers = Set.unions

-- Test if Board winso with given draws
boardWinner :: Board
            -> Set.Set Int -- draws
            -> Bool
boardWinner board draw = any id $ map rowWinner board
  where
    rowWinner :: Row -> Bool
    rowWinner row = Set.difference row draw == Set.empty

-- The numbers of a board that are not in the draws
boardUnmarked :: Board 
              -> Set.Set Int -- draws 
              -> Set.Set Int
boardUnmarked board = Set.difference (boardNumbers board)

for :: [a] -> (a -> b) -> [b]
for = flip map

-- Finds the draws for a bingo Board to Bingo!
boardBingo :: [Int] -> Board -> [Int]
boardBingo draws board =
  head . catMaybes $ for (tail $ inits draws) $ \draw ->
    let setDraw = Set.fromList draw
        drawn   = last draw
    in if boardWinner board setDraw then Just draw else Nothing

solve :: ([([Int], Board)] -> [([Int], Board)]) -- id to find first winner, reverse to find last winner
      -> [Int] -> [Board] -> Int
solve f draws boards = 
      -- Collect the draws until each board wins
  let boardsDraws = map (\b -> (boardBingo draws b, b)) boards
      -- Find winning board by sorting the list on shortest draw list (or losing board by reversing)
      winningBoard = head $ f $ sortOn (length . fst) boardsDraws
      -- Collect the numbers on the winning board which have not been drawn
      unmarked = boardUnmarked (snd winningBoard) (Set.fromList $ fst winningBoard)
  in sum (Set.toList unmarked) * last (fst winningBoard)

solve1, solve2 :: [Int] -> [Board] -> Int
solve1 = solve id
solve2 = solve reverse
  
main :: IO ()
main = do
  [((testDraws, testBoards), "")] <- readP_to_S parseBingo <$> readFile "testInput"
  [((draws, boards), "")] <- readP_to_S parseBingo <$> readFile "input"

  print $ solve1 testDraws testBoards
  print $ solve1 draws boards

  print $ solve2 testDraws testBoards
  print $ solve2 draws boards
