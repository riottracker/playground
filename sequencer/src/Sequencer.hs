{-# LANGUAGE RecordWildCards #-}

module Sequencer where

import qualified Sound.RtMidi as Rt
import           Foreign.C.Types
import           Data.Word
import           Data.Maybe
import           Control.Concurrent
import           Control.Concurrent.STM
import           Codec.Tracker.Common
import           Codec.Tracker.XM.Pattern

emptyCell = Cell Nothing Nothing Nothing Nothing Nothing

fromCell :: Cell -> [Foreign.C.Types.CUChar]
fromCell (Cell Nothing _ _ _ _) = []
fromCell (Cell (Just n) i v _ _) = case n of
  NoteCut -> [] 
  NoteOff -> []
  NoteFade -> []
  Note p -> [144, fromIntegral . fromEnum $ p, fromIntegral $ fromMaybe 127 v]

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
                    let ev = [[0xB0, 0x7B, 0x00]] ++ (fromCell <$> (map (!! position seq) (track (song seq))))
                    mapM_ (Rt.sendMessage (output seq)) ev
                    threadDelay $ 60000000 `div` (4 * (tempo seq)) -- YOLO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #WorksForMe
                    modifySequencer sequencer (\s -> s { position = if position seq < length (track (song seq) !! 0) - 1 then position seq + 1 else 0 })
        False -> return ()
