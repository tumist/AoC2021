import Control.Monad (forM)
import Data.Char (isDigit, digitToInt)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP

-- Re-use from Day 9
type Coord = (Int, Int)
x, y :: Coord -> Int
x = fst
y = snd
type EnergyLevels = Map.Map Coord Int

parseFile :: ReadP EnergyLevels
parseFile = do
    lns <- manyTill parseLine eof
    lst <- forM (zip [0..] lns) $ \(y, line) ->
        forM (zip [0..] line) $ \(x, digit) -> pure ((x,y), digit)
    pure $ Map.fromList (concat lst)
    where
        parseLine :: ReadP [Int]
        parseLine = do
            digits <- map digitToInt <$> munch1 isDigit
            char '\n'
            pure digits

-- For debugging
printEnergyLevels :: EnergyLevels -> IO ()
printEnergyLevels el = do
    let ((x,y), _) = Map.findMax el
    forM [0..y] $ \y -> do
        forM [0..x] $ \x -> putStr . pad . show $ el ! (x,y)
        putStrLn ""
    pure ()
    where
        pad s
          | length s == 1 = "  " ++ s
          | length s == 2 = ' ' : s
          | otherwise = s

energize :: EnergyLevels -> EnergyLevels
energize = Map.map (+1)

-- To get adjacent squares
adjacents :: Coord -> [Coord]
adjacents (x,y) = [(x+x',y+y') | x' <- [-1..1], y' <- [-1..1]]

-- Which octopuses flash
flash :: EnergyLevels -> [Coord]
flash = Map.keys . Map.filter (> 9)

-- Apply one round of flashing
applyFlash :: EnergyLevels 
           -> (EnergyLevels -- Resulting EnergyLevels 
              , Int)        -- Total number of flashes
applyFlash el =
    let (el', flashes) = applyFlash' (el, [])
    in (el', length flashes)

applyFlash' :: (EnergyLevels, [Coord]) -> (EnergyLevels, [Coord])
applyFlash' (el, alreadyFlashed) =
    let flashing = flash el
        adjacent = concatMap adjacents flashing
        energized = foldr (Map.adjust (+1)) el adjacent
        zeroed = foldr (Map.adjust (const 0)) energized (alreadyFlashed ++ flashing)
    in 
        -- If no octopuses flashed in this round, the step is complete
        if null flashing then
            (zeroed, alreadyFlashed)
        -- If some did flash, it may have increased the energylevel of
        -- other octopuses to cause a flash, so we run it again, accumulating flashers
        else
            applyFlash' (zeroed, alreadyFlashed ++ flashing)

step :: EnergyLevels -> (EnergyLevels, Int)
step = applyFlash . energize

steps :: EnergyLevels -> Int -> (EnergyLevels, Int)
steps el n = foldr sumStep (el, 0) [1..n]
    where
        sumStep :: Int -> (EnergyLevels, Int) -> (EnergyLevels, Int)
        sumStep _ (el, acc) =
            let (el', flashed) = step el
            in (el', acc + flashed)

-- Part 2: find the step causing all octopus to flash
solve2 :: EnergyLevels -> Int
solve2 el = fst $ until allFlashed enumSteps (0, (el, 0))
    where
        allFlashed (_, (_, flashes)) = flashes == 100
        enumSteps (steps, (el, _)) = (steps + 1, step el)

main :: IO ()
main = do
    [(testInput2, "")] <- readP_to_S parseFile <$> readFile "testInput2"
    [(input, "")] <- readP_to_S parseFile <$> readFile "input"
    print $ snd (steps testInput2 100)
    print $ snd (steps input 100)
    
    print $ solve2 testInput2
    print $ solve2 input