import Data.Bits
import Text.ParserCombinators.ReadP

hex2bin :: String -> String
hex2bin = concatMap hex2bin'
hex2bin' :: Char -> String
hex2bin' '0' = "0000"
hex2bin' '1' = "0001"
hex2bin' '2' = "0010"
hex2bin' '3' = "0011"
hex2bin' '4' = "0100"
hex2bin' '5' = "0101"
hex2bin' '6' = "0110"
hex2bin' '7' = "0111"
hex2bin' '8' = "1000"
hex2bin' '9' = "1001"
hex2bin' 'A' = "1010"
hex2bin' 'B' = "1011"
hex2bin' 'C' = "1100"
hex2bin' 'D' = "1101"
hex2bin' 'E' = "1110"
hex2bin' 'F' = "1111"
hex2bin' _ = error "Not hex"

bin2bits :: String -> Int
bin2bits s =
    let enumerated = zip [0..] (reverse s)
    in foldl (\bits (pos, bit) -> if bit == '1' then setBit bits pos else bits) zeroBits enumerated

data PacketValue
    = Literal Int
    | Operator Op [Packet]
    deriving Show

data Op
    = Sum | Product | Minimum | Maximum | Greater | Less | Equal
    deriving Show

data Packet
    = Packet
    { version :: Int
    , value :: PacketValue }
    deriving Show

parsePacketBin :: ReadP Packet
parsePacketBin = do
    version <- bin2bits <$> count 3 get
    typeId <- bin2bits <$> count 3 get
    value <- case typeId of
        0 -> parseOperator Sum
        1 -> parseOperator Product
        2 -> parseOperator Minimum
        3 -> parseOperator Maximum
        4 -> parseLiteral
        5 -> parseOperator Greater
        6 -> parseOperator Less
        7 -> parseOperator Equal
        _ -> pfail
    pure $ Packet version value

parseLiteral :: ReadP PacketValue
parseLiteral = Literal . bin2bits <$> parseVariableLength

parseVariableLength :: ReadP String
parseVariableLength = do
    continue <- (== '1') <$> get
    bin <- count 4 get
    if continue then do
        rest <- parseVariableLength
        pure $ bin ++ rest
    else
        pure bin

parseOperator :: Op -> ReadP PacketValue
parseOperator op = do
    lentyp <- parseLengthType
    case lentyp of
        Length n -> do
            bin <- count n get
            let [(pkts, _zeroes)] = readP_to_S (many1 parsePacketBin <* eof) bin
            pure $ Operator op pkts
        Packets n -> do
            pkts <- count n parsePacketBin
            pure $ Operator op pkts

data LengthOrPackets
    = Length Int
    | Packets Int

parseLengthType :: ReadP LengthOrPackets
parseLengthType = do
    lengthTypeId <- get
    case lengthTypeId of
        '0' -> do
            length <- bin2bits <$> count 15 get
            pure $ Length length
        '1' -> do
            pkts <- bin2bits <$> count 11 get
            pure $ Packets pkts
        _ -> pfail

versionSum :: Packet -> Int
versionSum (Packet v (Literal _)) = v
versionSum (Packet v (Operator _ pkts)) = v + sum (map versionSum pkts)

eval :: Packet -> Int
eval (Packet _ (Literal n)) = n
eval (Packet _ (Operator Sum pkts)) = sum $ map eval pkts
eval (Packet _ (Operator Product pkts)) = product $ map eval pkts
eval (Packet _ (Operator Minimum pkts)) = minimum $ map eval pkts
eval (Packet _ (Operator Maximum pkts)) = maximum $ map eval pkts
eval (Packet _ (Operator Greater (a:b:_))) = if eval a > eval b then 1 else 0
eval (Packet _ (Operator Less (a:b:_))) = if eval a < eval b then 1 else 0
eval (Packet _ (Operator Equal (a:b:_))) = if eval a == eval b then 1 else 0

main :: IO ()
main = do
    [(pkt, _)] <- readP_to_S parsePacketBin . hex2bin <$> readFile "input"
    print $ versionSum pkt
    print $ eval pkt