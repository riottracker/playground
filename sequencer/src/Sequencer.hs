{-# LANGUAGE RecordWildCards #-}

module Sequencer where

import           Sound.PortMidi

import           Data.Word
import           Control.Concurrent
import           Control.Concurrent.STM
import           MidiOutput

data Tone = Cn | C' | Dn | D' 
    | En 
    | Fn 
    | F' 
    | Gn 
    | G' 
    | An 
    | A' 
    | Bn 
    deriving (Enum,Eq)

instance Show Tone where
    show Cn = "C-"
    show C' = "C#"
    show Dn = "D-"
    show D' = "D#"
    show En = "E-"
    show Fn = "F-"
    show F' = "F#"
    show Gn = "G-"
    show G' = "G#"
    show An = "A-"
    show A' = "A#"
    show Bn = "B-"

data Pitch = Pitch
    { tone :: Tone
    , octave :: Int
    } deriving (((Eq)))

instance Show Pitch where
    show p = show (tone p) ++ show (octave p)

midiPitch :: Pitch -> Word8
midiPitch Pitch{..} = fromIntegral (octave * 12 + fromEnum tone)

data Cell = Cell
    { pitch :: Maybe Pitch
    , instrument :: Maybe Int
    , volpan :: Maybe Int
    , fxtype :: Maybe Int
    , fxparam :: Maybe Int
    } 

emptyCell :: Cell
emptyCell = Cell Nothing Nothing Nothing Nothing Nothing

data Song = Song
    { track :: [[Cell]]
    } 

data Sequencer = Sequencer
    { song :: Song
    , position :: Int
    , playing :: Bool
    , output :: PMStream
    , seqRedraw :: Bool
    } 

mkSequencer :: PMStream -> Sequencer
mkSequencer m = Sequencer defaultSong 0 False m True

defaultSong :: Song
defaultSong = 
    Song
    { track = replicate 4 (replicate 32 emptyCell)
    }

modifySequencer :: TVar Sequencer -> (Sequencer -> Sequencer) -> IO ()
modifySequencer seq f = atomically $ modifyTVar seq f

play :: TVar Sequencer -> IO ()
play seq = modifySequencer seq (\s -> s { playing = not (playing s)})

modifyCell :: TVar Sequencer -> (Cell -> Cell) -> Int -> Int -> IO ()
modifyCell seq f chn n = modifySequencer seq $ updateCell f chn n

updateCell :: (Cell -> Cell) -> Int -> Int -> Sequencer -> Sequencer
updateCell f chn n seq = 
    seq
    { song = (song seq)
      { track = updateNth chn (updateNth n f) trk
      }
    }
  where
    trk = track $ song seq
    updateNth n u (x:xs)
      | n == 0 = u x : xs
      | otherwise = x : updateNth (n - 1) u xs

tick :: TVar Sequencer -> IO ()
tick seq = do
    sequencer <- readTVarIO seq
    modifySequencer seq (\x -> if playing sequencer then x { seqRedraw = True } else x)
    
