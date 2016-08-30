module State where

import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM
import           Graphics.Vty

import           MidiOutput
import           Sequencer

data State = State { sequencer  :: TVar Sequencer
                   , thread     :: ThreadId
                   , cursorY    :: Int
                   , cursorX    :: Int
                   , editMode   :: Bool
                   , sChannel   :: Int
                   , sOctave    :: Int
                   , vty        :: Vty
                   }

defaultState :: Vty -> MidiOutput -> Song -> IO State
defaultState v m s = do seq <- newTVarIO (mkSequencer m) { song = s }
                        tid <- forkIO $ tick seq                        
                        return State { sequencer = seq
                                     , thread    = tid
                                     , cursorX   = 0
                                     , cursorY   = 0
                                     , editMode  = False
                                     , sChannel  = 0
                                     , sOctave   = 4
                                     , vty       = v
                                     }

withState :: Vty -> MidiOutput -> Song -> (State -> IO a) -> IO a
withState v m s f = defaultState v m s >>= f
              
readSong :: State -> IO Song
readSong s = liftM song (readTVarIO (sequencer s))

readPlaying :: State -> IO Bool
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

