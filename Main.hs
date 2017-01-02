module Main (main) where

import qualified Control.Exception as Exception
import qualified Data.Array.IO as Array
import qualified Data.ByteString as ByteString

type Msg = ByteString.ByteString

type Chan = Array.IOArray Int Msg

windowSize = 200000
msgCount = 1000000

message :: Int -> Msg
message n = ByteString.replicate 1024 (fromIntegral n)

pushMsg :: Chan -> Int -> IO ()
pushMsg chan highId = do
    m <- Exception.evaluate $ message highId
    Array.writeArray chan (highId `mod` windowSize) m

main :: IO ()
main = do
  c <- Array.newArray_ (0, windowSize)
  mapM_ (pushMsg c) [0..msgCount]
