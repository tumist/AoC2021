-- For the second part, a state called Aim is introduced and therefor
-- we can't treat each instruction as a delta position any more.
-- The state has been separated in to Position
-- and instructions get their own Instruction data type.
data Position = Pos Int -- Horizontal
                    Int -- Depth
                    Int -- Aim
                deriving Show

data Instruction
    = Forward Int
    | Up Int
    | Down Int

-- Now the sum function takes Instructions and gives a final Position.
-- This makes the folding function a bit more complicated, but at least
-- it's not so complicated to reach for a State monad.
sumInstruction :: [Instruction] -> Position
sumInstruction = foldl plusInstr (Pos 0 0 0)
  where
    plusInstr (Pos h d a) (Down n) = Pos h d (a + n)
    plusInstr (Pos h d a) (Up n) = Pos h d (a - n)
    plusInstr (Pos h d a) (Forward n) = Pos (h + n) (d + a * n) a

mulPos (Pos h d _) = h * d

parseFile :: FilePath -> IO [Instruction]
parseFile fp = do
    f <- fmap lines (readFile fp)
    return $ map (parseLine . words) f
    
parseLine :: [String] -> Instruction
parseLine ["forward", n] = Forward (read n)
parseLine ["down", n] = Down (read n)
parseLine ["up", n] = Up (read n)
parseLine e = error $ "parseLine: " ++ show e

main :: IO ()
main = do
    parseFile "testInput" >>= print . mulPos . sumInstruction
    parseFile "input" >>= print . mulPos . sumInstruction