{-# LANGUAGE RecordWildCards #-}

import Data.Maybe
import Stream
import Play
import Keys
import Synth
import Instrument
import Event
import SampleBank
import qualified Data.Map as M



sampleRate :: Int
sampleRate = 44100


type Song = [[(Event, Int)]]



exampleSong = [ [(NoteOn 67, 0), (NoteOn 0, 1)], [(NoteOn 0, 1)], [], [(NoteOn 65, 0)], [(NoteOn 1, 1)], [], [], []
              , [(NoteOn 65, 0)], [(NoteOn 0, 1)], [], [(NoteOn 64, 0)], [(NoteOn 1, 1)], [], [], []
              , [(NoteOn 63, 0)], [(NoteOn 0, 1)], [], [(NoteOn 62, 0)], [(NoteOn 1, 1)], [], [], [(NoteOn 58, 0)]
              , [(NoteOn 58, 0)], [(NoteOn 57, 0), (NoteOn 0, 1)], [(NoteOn 60, 0)], [(NoteOn 48, 0)], [(NoteOn 1, 1)], [], [], []
              ]

play :: Song -> Int -> M.Map Int Instrument -> Double -> IO ()
play s pos ins tempo = p >> if pos + 1 == length s then return ()
                      else play s (pos + 1) ni tempo
  where i = foldr u ins (s !! pos)
        u (ev,x) b = if isJust $ M.lookup x b then M.adjust (handleEvent ev) x b else b
        pairs = M.map (render tempo sampleRate) i
        ni = M.map fst pairs
        res = foldr (zipWith (+)) (repeat 0) $ M.elems $ M.map snd pairs
        p = playOnStdOut res

main = do
  bd <- loadSample "../sequencer/bd.wav" 0
  stick <- loadSample "../sequencer/stick.wav" 0
  let sb = SampleBank (M.fromList [(0, bd), (1, stick)]) 0 NoLoop Nothing
  let instruments = M.fromList [(0, Instrument bell), (1, Instrument sb)]
  play exampleSong 0 instruments 0.3
  

