module App where

import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM
import           Graphics.Vty

import           MidiOutput
import           Sequencer

data App = App { sequencer  :: TVar Sequencer
               , thread     :: ThreadId
               , cursorY    :: Int
               , cursorX    :: Int
               , editMode   :: Bool
               , sChannel   :: Int
               , sOctave    :: Int
               , vty        :: Vty
               }

defaultApp :: Vty -> MidiOutput -> Song -> IO App
defaultApp v m s = do seq <- newTVarIO (mkSequencer m) { song = s }
                      tid <- forkIO $ tick seq                        
                      return App { sequencer = seq
                                 , thread    = tid
                                 , cursorX   = 0
                                 , cursorY   = 0
                                 , editMode  = False
                                 , sChannel  = 0
                                 , sOctave   = 4
                                 , vty       = v
                                 }

withApp :: Vty -> MidiOutput -> Song -> (App -> IO a) -> IO a
withApp v m s f = defaultApp v m s >>= f
              
readSong :: App -> IO Song
readSong s = liftM song (readTVarIO (sequencer s))

readPlaying :: App -> IO Bool
readPlaying s = liftM playing (readTVarIO (sequencer s))     

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

