module Main (main) where
  
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Array.IO as Array
import qualified Data.ByteString as ByteString
import qualified Data.Time.Clock as Clock

type Msg = ByteString.ByteString

type Chan = Array.IOArray Int Msg

windowSize = 200000
msgCount = 1000000

message :: Int -> Msg
message n = ByteString.replicate 1024 (fromIntegral n)

pushMsg :: Chan -> Clock.NominalDiffTime -> Int -> IO Clock.NominalDiffTime
pushMsg chan worst highId = do
    start <- Clock.getCurrentTime
    m <- Exception.evaluate $ message highId
    Array.writeArray chan (highId `mod` windowSize) m
    end <- Clock.getCurrentTime
    let elapsed = Clock.diffUTCTime end start
    return $ max elapsed worst

main :: IO ()
main = do
  c <- Array.newArray_ (0, windowSize)
  worst <- Monad.foldM (pushMsg c) 0 [0..msgCount]
  print worst
