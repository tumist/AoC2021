{-# LANGUAGE ScopedTypeVariables #-}
import Data.List (sort)

testPositions :: [Int]
testPositions = [16,1,2,0,4,2,7,1,2,14]

totalFuelAtPos :: (Int -> Int) -- Fuel rate function. `id` for part 1, `fuelRate` for part 2
               -> [Int]        -- Crabs
               -> Int          -- Position for all crabs to move to
               -> Int          -- Total fuel
totalFuelAtPos fr crabs pos = sum $ map (fr . abs . (\n -> n - pos)) crabs

-- Part 1: Assuming that the middle value of the sorted crab position works
--         for both testInput and real input, but I don't know why :x
solve :: [Int] -> Int
solve ps = let listMiddle = length ps `div` 2
               middle = sort ps !! listMiddle
           in totalFuelAtPos id ps middle

-- Part 2: Search for local minima
localMin :: (Int -> Int) -> [Int] -> Int
localMin _ [x] = x
localMin f [x,y] = if f x < f y then x else y
localMin f l =
    let (a, b) = splitAt (length l `div` 2) l
    in if f (last a) < f (head b) then localMin f a else localMin f b

fuelRate :: Int -> Int
fuelRate n = (n * n + n) `div` 2

solve2 :: [Int] -> Int
solve2 ps =
  let sorted = sort ps
      searchPositions = [head sorted .. last sorted]
      res = localMin (totalFuelAtPos fuelRate ps) searchPositions
  in totalFuelAtPos fuelRate ps res

main :: IO ()
main = do
  positions :: [Int] <- read <$> readFile "input"
  print $ solve testPositions
  print $ solve positions
  print $ solve2 testPositions
  print $ solve2 positions
  