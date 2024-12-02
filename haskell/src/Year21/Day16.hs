
module Year21.Day16 (run) where

import qualified Data.Text as T
import Data.Text.Read (hexadecimal)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Char8 as C
import Data.Word (Word8)
import Data.Bits
import Data.List (unfoldr)

data Header = Header { packetVersion :: Word8
                     , packetType :: Word8
                     } deriving (Show)
  
data Packet = Operator { header :: Header
                       , lenId :: Word8
                       , len :: Int
                       , packets :: [Packet]
                        }
            | Literal { header :: Header
                      , value :: Integer
            }

instance Show Packet where
  show (Operator hd li ln packets) = "Op" ++ show (packetType hd) ++ " v" ++ show (packetVersion hd) ++ ": (" ++ show li ++ "|" ++ show ln ++ ") - " ++ show packets
  show (Literal hd value) = "Lit" ++ show (packetType hd) ++ " v" ++ show (packetVersion hd) ++": " ++ show value

tape :: C.ByteString -> [Word8]
tape = concatMap (reverse.take 8.writeBinary.hex).T.chunksOf 2.decodeUtf8

writeBinary :: Bits a => a -> [a] 
writeBinary b = map (\i -> shiftR b i .&. bit 0) [0..]

readBinary :: Integral a => [a] -> Integer
readBinary = foldl (\a (i, b) -> a + shiftL 1 i * toInteger b) 0.zip [0..].reverse

hex :: T.Text -> Word8
hex t = case hexadecimal t of 
  Right (h, _) -> h
  Left _ -> 0
        
readHeader :: [Word8] -> (Header, [Word8])
readHeader tape = (Header pVersion pType, tape')
  where (header, tape') = splitAt 6 tape
        (bVersion, bType) = splitAt 3 header
        pVersion = fromInteger.readBinary $ bVersion
        pType = fromInteger.readBinary $ bType

readPacket :: [Word8] -> (Packet, [Word8])
readPacket tape 
  | packetType header == 4 = readLiteral header tape'
  | otherwise = case head tape' of 
    0 -> readOp0 header tape'
    _ -> readOp1 header tape'
  where (header, tape') = readHeader tape

readLiteral :: Header -> [Word8] -> (Packet, [Word8])
readLiteral header tape = (Literal header v, tape')
  where chunks = map (take 5).iterate (drop 5) $ tape
        vchunks = (\(as ,b:bs) -> as ++ [b]).break (\(a:_) -> a == 0) $ chunks
        v = readBinary $ foldr (\(a:as) acc -> as++acc) [] vchunks
        tape' = drop (5*length vchunks) tape

readOp0 :: Header -> [Word8] -> (Packet, [Word8])
readOp0 header tape = (Operator header lenId len packets, tape')
  where lenId = head tape
        len = fromInteger.readBinary.take 15.tail $ tape 
        content = take len.drop 16 $ tape
        packets = unfoldr (\tp -> if null tp then Nothing else Just $ readPacket tp) content
        tape' = drop (len+16) tape

readOp1 :: Header -> [Word8] -> (Packet, [Word8])
readOp1 header tape = (Operator header lenId len (reverse packets), tape')
  where lenId = head tape
        len = fromInteger.readBinary.take 11.tail $ tape  
        content = drop 12 tape
        step = \(ps, tp) -> let (p, tp') = readPacket tp in (p:ps,tp')
        (packets, tape') = iterate step ([], content) !! len

count :: Packet -> Integer 
count (Operator hd _ _ ps) = (toInteger.packetVersion $ hd) + foldr ((+).count ) 0 ps
count (Literal hd _) = toInteger.packetVersion $ hd

eval :: Packet -> Integer
eval (Literal _ v) = v
eval (Operator hd _ _ ps) = case packetType hd of
  0 -> sum eps
  1 -> product eps
  2 -> minimum eps
  3 -> maximum eps
  5 -> if (eps !! 0) > (eps !! 1)  then 1 else 0
  6 -> if (eps !! 0) < (eps !! 1)  then 1 else 0
  _ -> if (eps !! 0) == (eps !! 1) then 1 else 0
  where eps = map eval ps

run :: C.ByteString -> Integer
run t = eval packet
  where tp = tape t
        (packet,_) = readPacket tp
