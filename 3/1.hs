{-# LANGUAGE RecordWildCards #-}
import Data.List ( transpose )
import Data.Bits ( Bits, zeroBits, setBit, testBit )

-- Parse every bit string as 0b0 being False and 0b1 being True.
-- Is it big-endian or little-endian? SANTA KNOWS!
type BitList = [Bool]
type BitEnumerated = [(Int, Bool)] -- [(bit position, whether the bit is set)]

parseFile :: FilePath -> IO ([BitList], Int)
parseFile fp = do
  lns <- lines <$> readFile fp
  let width = length $ head lns
  pure (map (map chrInt) lns, width)
  where
    chrInt '0' = False
    chrInt '1' = True
    chrInt c   = error $ "Unexpected character: " ++ show c

-- Enumerates the bits (with the last bit being index 0)
bitListNum :: BitList -> Int
bitListNum = bitsToNum . zip [0..] . reverse
-- Constructs a Num starting with zeroBits and setting the bits at each position
bitsToNum :: BitEnumerated -> Int
bitsToNum = foldr (\(pos, bit) -> if bit then (`setBit` pos) else id) zeroBits

-- To get majority of a bitstring, we can map False to -1 and True to 1
-- and sum the list. If the sum is positive, there are more 1's than 0's.
bitMajority :: BitList -> Int
bitMajority = sum . map bitToInt
  where
    bitToInt False = -1
    bitToInt True  = 1

-- To sum each column in the bit string input, transpose the parsed data
-- and sum the row. Also enumerate the column number:
-- The first column (list index 0) will become the most significant bit in
-- gamma and epsilon, and the last column the least significant bit.
-- To tag the last column as bit number 0, it's reversed before being enumerated.
sumTrans :: [BitList] -> [(Int, Int)]
sumTrans = zip [0..] . reverse . map bitMajority . transpose

-- We can now construct gamma and epsilon using `bitsToNum`
gamma, epsilon :: [(Int, Int)] -> Int
gamma   = bitsToNum . map (\(pos, sum) -> (pos, sum > 0))
epsilon = bitsToNum . map (\(pos, sum) -> (pos, sum < 0))

solve1 :: [BitList] -> Int
solve1 bs =
  let summed = sumTrans bs
  in gamma summed * epsilon summed


-- Part 2 --
data State = State 
  { numbers :: [Int]
  -- The current column of bits of which to determine majority and filter numbers.
  -- Set to BitList width - 1 at start.
  , bitPosition :: Int
  }
  deriving Show

oneLeft :: State -> Bool
oneLeft = (==) 1 . length . numbers

reduce :: (Bool -> Bool) -> State -> State
reduce flipMajor State{..} =
      -- Count how many bits are set, or not set, at `bitPosition`
  let setBits = filter (\b -> testBit b bitPosition == True) numbers
      notBits = filter (\b -> testBit b bitPosition == False) numbers
      -- Determines which is majority
      majority = length setBits >= length notBits
      -- The reduced state has all numbers outside of majority removed
      -- and bitPosition lowered.
      -- The comparison can be negated for for calculating the co2 scrubber rating
  in State { numbers = filter (\b -> testBit b bitPosition == flipMajor majority) numbers
           , bitPosition = bitPosition - 1}

solve2 :: [BitList] -> Int -> Int
solve2 bs width =
  let numbers = map bitListNum bs
      initialState = State numbers (width - 1)
      --
      State { numbers = [o] } = until oneLeft (reduce id) initialState
      State { numbers = [c] } = until oneLeft (reduce not) initialState
  in o * c

main :: IO ()
main = do
  (testInput, testWidth) <- parseFile "testInput"
  (input, width) <- parseFile "input"

  print $ solve1 testInput
  print $ solve1 input

  print $ solve2 testInput testWidth
  print $ solve2 input width
