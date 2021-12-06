import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict

type Fish = Int

parseNums :: ReadP [Fish]
parseNums = do
  nums <- sepBy (readS_to_P reads) (char ',')
  optional (char '\n')
  eof
  pure nums

-- Part 1: easy recursion

days :: Int  -- Days
     -> Fish -- Fish timer
     -> Int  -- Total fish
days 0 f = 1
days d 0 = days (d-1) 6 + days (d-1) 8
days d f = days (d-1) (f-1)

solve1 :: Int -> [Fish] -> Int
solve1 d = sum . map (days d)

-- Part 2: memoization approach

stateMemoDays :: Int -> Fish -> State (Map.Map (Int, Fish) Int) Int
stateMemoDays 0 f = return 1
stateMemoDays d 0 = lookUpdate (d,0) $ liftM2 (+) (stateMemoDays (d-1) 6) (stateMemoDays (d-1) 8)
stateMemoDays d f = lookUpdate (d,f) $ stateMemoDays (d-1) (f-1)

lookUpdate :: (Int, Fish) -> State (Map.Map (Int, Fish) Int) Int -> State (Map.Map (Int, Fish) Int) Int
lookUpdate key f = do
  memo <- gets (Map.lookup key)
  case memo of
    Just v -> return v
    Nothing -> do
      v <- f
      modify $ Map.insert key v
      return v

-- Fold stateMemoDays, accumulating the number of fish and the memo Map
solve2 :: Int -> [Fish] -> Int
solve2 d = fst . foldr f (0, Map.empty) -- Start with 0 Fish and empty memo Map
  where
    f :: Fish -- Fish timer at input
      -> (Int, Map.Map (Fish, Int) Int) -- Accumulated (Fish, memo Map)
      -> (Int, Map.Map (Fish, Int) Int) -- (summed Fish, expanded memo Map)
    f a (f', m) = let (a', m') = runState (stateMemoDays d a) m in (f' + a', m')

main :: IO ()
main = do
  [(testFish, "")] <- readP_to_S parseNums <$> readFile "testInput"
  [(fish, "")] <- readP_to_S parseNums <$> readFile "input"
  print $ solve1 80 testFish
  print $ solve1 80 fish

  print $ solve2 256 testFish
  print $ solve2 256 fish
