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
printDeviceList l = do
  putStrLn " Port    Client name                      Port name                Caps"
  mapM_ (\x -> putStrLn $
                   (interface x) ++ " "
                ++ (name x) ++ " "
                ++ (if input x then "in" else if output x then "out" else "  ")) l

getDefaultMidiOutput :: IO (Either PMStream PMError) 
getDefaultMidiOutput = getDeviceList >>= printDeviceList >> do
  putStrLn "Selection:"
  l <- getLine
  openInput ((read l) :: DeviceID)
