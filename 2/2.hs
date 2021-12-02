data Position = Pos Int -- Horizontal
                    Int -- Depth
                    Int -- Aim
                deriving Show

data Instruction
    = Forward Int
    | Up Int
    | Down Int

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