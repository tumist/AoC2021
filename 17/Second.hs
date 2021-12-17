{-# LANGUAGE TupleSections #-}
import Data.Maybe (mapMaybe)
import Data.List (elemIndex)
import qualified Data.Set as Set

-- Given a y in target, these y'-velocity values will reach it:
--  y' = y,  
--  (y' + (y'-1)) = y,                     y' = (y+1)/2
--  (y' + (y'-1) + (y'-2)) = y             y' = (y+3)/3
--  (y' + (y'-1) + (y'-2) + (y'-3)) = y    y' = (y+6)/4
-- and so on, where there are integer solutions for y' and y' >= 1

type Steps = Int

-- 0, 1, 3, 6, 10 ...
sumSeries :: [Int]
sumSeries = scanl (+) 0 [1..]

solveY :: Int -> [(Steps, Int)]
solveY y = mapMaybe f (takeWhile (\(sums,steps) -> sums `div` steps <= abs y) $ zip sumSeries [1..])
    where
        f (sums, steps) =
            case (y + sums) `divMod` steps of
                (cleanDiv, 0) -> Just (steps, cleanDiv)
                _ -> Nothing

-- Given an x target coordinate, is there an x'-velocity coordinate 
-- that will get me there in `s` steps?
-- We can put the numbers directly in for the equations in explanation for `solveY`, but!
-- since the rocket does not start to go in the negative direction again,
-- they don't apply when steps > x'-velocity
solveX :: Int -> Steps -> Maybe Int
solveX x steps =
    -- The rocket slows down x'-velocity and once it stops, any step after that is reachable
    -- This happens only at the numbers in sumSeries
    let sumSeriesX = takeWhile (<= x) sumSeries
        idx = elemIndex x sumSeriesX
    in case idx of
        Just i -> if i <= steps then Just i else checkDiv
        Nothing -> checkDiv
    where
        checkDiv =
            let p = sumSeries !! (steps - 1)
            in case (x + p) `divMod` steps of
                (cleanDiv, 0) -> if steps > cleanDiv then Nothing else Just cleanDiv
                _ -> Nothing

-- List of ever coordinate in the target
targetCoords :: Int -> Int -> Int -> Int -> [(Int, Int)]
targetCoords x1 x2 y1 y2 = [(x,y) | x <- [x1..x2], y <- [y1..y2]]

-- Combining solveY with solveX to find what launches will end up in coordinate
launchesTo :: (Int, Int) -> [(Int, Int)]
launchesTo (x, y) = mapMaybe (\(s, y') -> (,y') <$> solveX x s) (solveY y)

launchesToTarget :: Int -> Int -> Int -> Int -> Set.Set (Int, Int)
launchesToTarget x1 x2 y1 y2 = Set.fromList $ concatMap launchesTo (targetCoords x1 x2 y1 y2)

main :: IO ()
main = do
    print $ Set.size $ launchesToTarget 20 30 (-10) (-5)
    print $ Set.size $ launchesToTarget 192 251 (-89) (-59)