module Main (main) where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map

type Msg = ByteString.ByteString

type Chan = Map.Map Int Msg

windowSize = 200000
msgCount = 1000000

message :: Int -> Msg
message n = ByteString.replicate 1024 (fromIntegral n)

pushMsg :: Chan -> Int -> IO Chan
pushMsg chan highId =
  Exception.evaluate $
    let lowId = highId - windowSize in
    let inserted = Map.insert highId (message highId) chan in
    if lowId < 0 then inserted
    else Map.delete lowId inserted

main :: IO ()
main = Monad.foldM_ pushMsg Map.empty [0..msgCount]

