module Main where

--import           Control.Monad.Loops
import qualified Sound.RtMidi as Rt
import           Data.Maybe
import           Data.Char
import           Graphics.Vty

import           App
import           Images
import           Editor

import Sequencer
import Control.Monad.IO.Class (liftIO, )
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Debug.Trace
import Codec.Tracker.Common
import Codec.Tracker.XM.Pattern

main = do
    device <- Rt.defaultOutput
    numPorts <- Rt.portCount device
    ports <- mapM (Rt.portName device) [0..numPorts-1]
    mapM_ (\t -> putStrLn $ show t) $ zip [0..] ports
    putStrLn "select port: "
    selection <- getLine
    Rt.openPort device (read selection) "riottracker"
    config <- standardIOConfig
    vty <- mkVty config
    withApp vty device exampleSong run

checkA :: App -> IO Bool
checkA a = do x <- readTVarIO (editor a)
              return $ quitEditor x

run app = do
  renderThread <- forkIO $ forever $ render app
  let k = do handleEvents app 
             e <- readTVarIO (editor app)
             if quitEditor e then return () else k
  k

exampleSong :: Song
exampleSong = Song [
                     [ Cell (Just $ Note (Pitch C 4)) (Just 1) Nothing Nothing Nothing
                     , emptyCell
                     , Cell (Just $ Note (Pitch D 4)) (Just 1) Nothing Nothing Nothing
                     , emptyCell
                     , Cell (Just $ Note (Pitch E 4)) (Just 1) Nothing Nothing Nothing
                     , emptyCell
                     , Cell (Just $ Note (Pitch F 4)) (Just 1) Nothing Nothing Nothing
                     , emptyCell
                     , Cell (Just $ Note (Pitch G 4)) (Just 1) Nothing Nothing Nothing
                     , emptyCell
                     , emptyCell
                     , emptyCell
                     , Cell (Just $ Note (Pitch G 4)) (Just 1) Nothing Nothing Nothing
                     , emptyCell
                     , emptyCell
                     , emptyCell
                     ]
                     , replicate 16 emptyCell
                   ]


