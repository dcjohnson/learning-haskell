import Data.Word
import Data.Bits
import Data.Maybe
import qualified Data.ByteString as Bs
import qualified System.IO as Io

type Memory = [Word8]

defaultTestMemory1 :: Memory
defaultTestMemory1 = [1 | _ <- [1..10]]

abString :: Memory
abString = [66, 65, 66, 65, 66, 65, 66, 65, 66, 65, 66, 65, 66, 65, 66, 65, 66, 65, 66, 65, 66, 65]

main = do
  print $ defaultTestMemory1 !! 1
  print $ ((shiftL (fromIntegral (defaultTestMemory1 !! 2)) 8) :: Word32)
  print $ (Bs.pack [65 | _ <- [1..10]])
  print $ Bs.split 65 (Bs.pack abString)
  print $ fromJust $ readWord32FromMemory defaultTestMemory1 5
  memory <- readFileToMemory "test.txt"
  Bs.putStr $ Bs.pack memory
  print $ fromJust $ readWord32FromMemory memory 11
  let w = safeIndex defaultTestMemory1 8
  print $ if isJust w then fromJust w else 0

readFileToMemory :: String -> IO Memory
readFileToMemory path = do
  handle <- Io.openFile path Io.ReadMode
  contents <- Bs.hGetContents handle -- reads into ByteString. The IO version will read into a string
  Io.hClose handle
  return $ byteStringToMemory contents

-- Bs.ByteString is just a [Word8] like Memory. So unpacking will convert it to [Word8] then the type system will do the rest
byteStringToMemory :: Bs.ByteString -> Memory
byteStringToMemory b = Bs.unpack b

readWord32FromMemory :: Memory -> Int -> Maybe Word32
readWord32FromMemory m i = do
  if (i + 3) >= (length m) then Nothing
  else let w0 = shiftL (fromIntegral (m !! i)) 24
           w1 = shiftL (fromIntegral (m !! (i + 1))) 16
           w2 = shiftL (fromIntegral (m !! (i + 2))) 8
           w3 = fromIntegral (m !! (i + 3))
           in Just $ w0 .|. (w1 .|. (w2 .|. w3))

safeIndex :: Memory -> Int -> Maybe Word8
safeIndex m i = do
  if (i < 0) || (i >= (length m)) then Nothing
  else Just $ m !! i
