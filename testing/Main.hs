{-# LANGUAGE RecordWildCards #-}

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


type Song = [Maybe Event]



exampleSong = [ Just (NoteOn 67), Nothing, Nothing, Just (NoteOn 65), Nothing, Nothing, Nothing, Nothing
              , Just (NoteOn 65), Nothing, Nothing, Just (NoteOn 64), Nothing, Nothing, Nothing, Nothing
              , Just (NoteOn 63), Nothing, Nothing, Just (NoteOn 62), Nothing, Nothing, Nothing, Just (NoteOn 58)
              , Just (NoteOn 58), Just (NoteOn 57), Just (NoteOn 60), Just (NoteOn 48), Nothing, Nothing, Nothing, Nothing
              ]

pattern = [ Just (NoteOn 0), Nothing, Just (NoteOn 1), Nothing
          , Just (NoteOn 2), Nothing, Just (NoteOn 1), Nothing
          ]

play :: Instrument i1 => Song -> Int -> i1 -> Double -> IO ()
play s pos ins tempo = p >> if pos + 1 == length s then return ()
                      else play s (pos + 1) (fst k) tempo
  where i = case s !! pos of
               Just e  -> handleEvent ins e
               Nothing -> ins
        k = render i tempo sampleRate
        p = playOnStdOut $ snd k

main = do
--     play exampleSong 0 bell 0.3
  s <- loadSample "sample.wav" 0
  let s0 = take 3000 $ drop (sampleRate * 2) s
  let s1 = take 3000 $ drop (sampleRate * 10) s
  let s2 = take 3000 $ drop (sampleRate * 12) s
  let sb = SampleBank (M.fromList [(0, s0), (1, s1), (2, s2)]) 0 NoLoop Nothing
  play (take 120 $ cycle pattern) 0 sb 0.3
  

