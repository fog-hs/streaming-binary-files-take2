{-# Language AllowAmbiguousTypes,BangPatterns,RankNTypes,TypeApplications,ScopedTypeVariables #-}

import Data.ByteString.Lazy (hPut,hGetContents,ByteString)
import System.IO (openBinaryFile,Handle,IOMode(..),hClose)
import Data.Binary
import Stream.ListT
import Stream.LinearM


writeBinaryStream :: Binary a => FilePath -> [a] -> IO ()
writeBinaryStream path xs = do
 h <- openBinaryFile path AppendMode
 writeBinaryStream' h xs  
 hClose h
  where
   writeBinaryStream' h [] = return ()
   writeBinaryStream' h (x:xs) = do
    !_ <- hPut h (encode x) 
    writeBinaryStream' h xs

writeBinaryChunks :: Binary a => Int -> FilePath -> [a] -> IO ()
writeBinaryChunks i f xs = writeBinaryChunksM i f (liftList xs)

{-
writeBinaryStreamM :: Binary a => FilePath -> IOList a -> IO ()
writeBinaryStreamM path xs = do
 h <- openBinaryFile path AppendMode
 consume (hPut h . encode ) xs
 hClose h
-}

writeBinaryChunksM :: forall a. Binary a => Int -> FilePath -> IOList a -> IO ()
writeBinaryChunksM i path xs = do
 h <- openBinaryFile path AppendMode
 discard $ fst $ scannerM f (0,h) xs
 hClose h
  where
   open = openBinaryFile path AppendMode
   f :: (Int, Handle) -> a -> IO ((), (Int, Handle)) 
   f (n,h) a = do
    hPut h (encode a) 
    let b = 0 == mod n i
    if not b 
     then return ((),(n+1,h))
     else do
      hClose h
      !h' <- open
      return ((),(n+1,h'))


test = do
 h <- openBinaryFile "testRandoms.bin" AppendMode
 writeBinaryStream' h (1000000 :: Int)
 hClose h
  where
   writeBinaryStream' h 0 = return ()
   writeBinaryStream' h n = do
    !_ <- hPut h (encode n) 
    writeBinaryStream' h (n-1)



unConsBinaryStream :: forall a m. (Binary a,Monad m) => ByteString
           -> m (Maybe (a, ByteString))
unConsBinaryStream xs = case 
    decodeOrFail @a xs
     of
      Left _ -> return Nothing
      Right (xs,_,a) -> return (Just (a,xs))

readBinaryStream :: forall a. Binary a => FilePath -> IOList a
readBinaryStream path = unfoldrM unConsBinaryStream (openBinaryFile path ReadMode >>= hGetContents)
{-                  
copyBinaryStream :: forall a. Binary a => FilePath -> FilePath -> IO ()
copyBinaryStream path path2 = writeBinaryStreamM path2 $ readBinaryStream @a path
-}
copyBinaryChunks :: forall a. Binary a => Int -> FilePath -> FilePath -> IO ()
copyBinaryChunks i path path2 = writeBinaryChunksM i path2 $ readBinaryStream @a path

testWrite = writeBinaryStream "test.bin" [1:: Int .. 10000000]
testRead = display $ readBinaryStream @Int "test.bin" 
testCopy = copyBinaryChunks @Int 100000 "test.bin" "copy.bin" 
