module MidiOutput where

import Sound.PortMidi

import Text.Printf (printf, )
import Control.Monad (liftM, liftM2, liftM5, join, zipWithM_ )
import Control.Monad.IO.Class (liftIO, )
import Control.Monad.Trans.Cont     (ContT(ContT), runContT)
import System.Environment (getArgs, )
import Debug.Trace

import qualified System.Exit as Exit
import qualified System.IO as IO

getDeviceList :: IO [DeviceInfo]
getDeviceList = do
  d <- countDevices
  mapM getDeviceInfo [0..d-1]

printDeviceList :: [DeviceInfo] -> IO ()
printDeviceList l = mapM_ (\(i,x) -> printf "%3i %-40s %-20s %s%s\n" (i::Int) (name x) (interface x) (if input x then "in" else "") (if output x then "out" else "")) $ zip  [0..] l

getDefaultMidiOutput :: IO (Either PMStream PMError) 
getDefaultMidiOutput = getDeviceList >>= printDeviceList >> do
  putStrLn "Selection:"
  s <- getLine
  putStrLn "latency:"
  l <- getLine
  openOutput ((read s) :: DeviceID) ((read l) :: Int)
