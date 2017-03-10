{-# LANGUAGE RecordWildCards #-}

module Sequencer where

import qualified Sound.RtMidi as Rt
import           Foreign.C.Types
import           Data.Word
import           Data.Maybe
import           Control.Concurrent
import           Control.Concurrent.STM

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

midiPitch :: Pitch -> Foreign.C.Types.CUChar
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
    , tempo :: Int            -- BPM
    , playing :: Bool
    , output :: Rt.Device
    } 

mkSequencer :: Rt.Device -> Sequencer
mkSequencer m = Sequencer defaultSong 0 120 True m

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
tick sequencer = do
    seq <- readTVarIO sequencer
    case playing seq of
        True -> do
                    let ev = [[0xB0, 0x7B, 0x00]] ++ map (\p -> [144, midiPitch p, 127]) (catMaybes (map pitch (map (!! position seq) (track (song seq)))))
                    mapM_ (Rt.sendMessage (output seq)) ev
                    threadDelay $ 60000000 `div` (4 * (tempo seq)) -- YOLO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #WorksForMe
                    modifySequencer sequencer (\s -> s { position = if position seq < length (track (song seq) !! 0) - 1 then position seq + 1 else 0 })
        False -> return ()
