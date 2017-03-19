module Main where

import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment(getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)

main :: IO ()
main = withSocketsDo $ do 
  args <- getArgs
  let port = fromIntegral (read $ head args :: Int)
  sock <- listenOn $ PortNumber port
  putStrLn $ "Listen on " ++ (head args)
  sockHandler sock

sockHandler :: Socket -> IO()
sockHandler sock = do
  (handle, _, _) <- accept sock
  hSetBuffering handle NoBuffering
  forkIO $ commandProcessor handle
  sockHandler sock

commandProcessor :: Handle -> IO()
commandProcessor handle = do
  line <- hGetLine handle
  let cmd = words line
  case (head cmd) of
    ("echo") -> hPutStrLn handle (unwords $ tail cmd)
    _ -> do hPutStrLn handle "Command unknown"
  commandProcessor handle
