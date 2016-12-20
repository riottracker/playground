module Main where

import           Data.Maybe
import           Data.Char
import           Graphics.Vty

import           App
import           Images

import MidiOutput
import Sequencer
import Control.Monad.IO.Class (liftIO, )
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Debug.Trace

main = do
    listPorts
    putStr "Select port: "
    port <- getLine
    config <- standardIOConfig
    vty <- mkVty config
    withMidiOutput port $ \m -> withApp vty m exampleSong run


ethread app = do
  handleEvents app
  ethread app

run app = do
  eventThread <- forkIO $ ethread app
  main' app

main' app = do
    render app
    main' app



exampleSong :: Song
exampleSong = Song [
                     [ Cell (Just (Pitch Cn 4)) (Just 1) Nothing Nothing Nothing
                     , emptyCell
                     , Cell (Just (Pitch Dn 4)) (Just 1) Nothing Nothing Nothing
                     , emptyCell
                     , Cell (Just (Pitch En 4)) (Just 1) Nothing Nothing Nothing
                     , emptyCell
                     , Cell (Just (Pitch Fn 4)) (Just 1) Nothing Nothing Nothing
                     , emptyCell
                     , Cell (Just (Pitch Gn 4)) (Just 1) Nothing Nothing Nothing
                     , emptyCell
                     , emptyCell
                     , emptyCell
                     , Cell (Just (Pitch Gn 4)) (Just 1) Nothing Nothing Nothing
                     , emptyCell
                     , emptyCell
                     , emptyCell
                     ]
                     , replicate 16 emptyCell
                   ]


