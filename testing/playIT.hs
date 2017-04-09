{-# LANGUAGE RecordWildCards #-}

import Data.Maybe
import Stream
import Play
import Keys
import Synth
import Instrument
import Event
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get

import Codec.Tracker.IT
import Codec.Tracker.IT.Pattern

sampleRate :: Int
sampleRate = 44100


type Song = [[(Event, Int)]]

play :: Song -> Int -> M.Map Int Instrument -> Double -> IO ()
play s pos ins tempo = p >> if pos + 1 == length s then return ()
                      else play s (pos + 1) ni tempo
  where i = foldr u ins (s !! pos)
        u (ev,x) b = if isJust $ M.lookup x b then M.adjust (handleEvent ev) x b else b
        pairs = M.map (render tempo sampleRate) i
        ni = M.map fst pairs
        res = foldr (zipWith (+)) (repeat 0) $ M.elems $ M.map snd pairs
        p = playOnStdOut res

cell2Event :: Cell -> Maybe Event
cell2Event (Cell _ _ Nothing    _ _ _) = Nothing
cell2Event (Cell _ _ (Just 255) _ _ _) = Just NoteOff
cell2Event (Cell _ _ (Just 254) _ _ _) = Just NoteOff
cell2Event (Cell _ _ (Just n)   _ _ _) = Just (NoteOn $ fromIntegral n)
cell2Event _                           = Nothing

row2row :: [Cell] -> [(Event, Int)]
row2row cells = zip (catMaybes (map cell2Event cells)) [0..]

main = do
  file <- BL.getContents
  let it = runGet getModule file
  let instruments = M.fromList [ (0, Instrument bell)
                               , (1, Instrument bell)
                               , (2, Instrument bell)
                               , (3, Instrument bell)
                               , (4, Instrument bell)
                               ]
  let song = foldr (++) [] $ map ((map row2row) . rows) $ patterns it
  play song 0 instruments 0.1
  

