{-# LANGUAGE RecordWildCards #-}

import Stream
import Play
import Keys
import Synth
import Instrument
import Event

sampleRate :: Int
sampleRate = 44100


type Song = [Maybe Event]



exampleSong = [ Just (NoteOn 67), Nothing, Nothing, Just (NoteOn 65), Nothing, Nothing, Nothing, Nothing
              , Just (NoteOn 65), Nothing, Nothing, Just (NoteOn 64), Nothing, Nothing, Nothing, Nothing
              , Just (NoteOn 63), Nothing, Nothing, Just (NoteOn 62), Nothing, Nothing, Nothing, Just (NoteOn 58)
              , Just (NoteOn 58), Just (NoteOn 57), Just (NoteOn 60), Just (NoteOn 48), Nothing, Nothing, Nothing, Nothing
              ]

play :: Instrument i1 => Song -> Int -> i1 -> IO ()
play s pos ins = p >> if pos + 1 == length s then return ()
                      else play s (pos + 1) (fst k)
  where i = case s !! pos of
               Just e  -> handleEvent ins e
               Nothing -> ins
        k = render i 0.3 sampleRate
        p = playOnStdOut $ snd k

main = do
--   s <- loadStream "sample.wav" 1
--   let schnipsel = take 140000 s
--   playOnStdOut $ schnipsel ++ (reverse schnipsel) ++ pause
--   playOnStdOut $ concat $ map (renderSignal 0.0 0.4 sampleRate) $ map (sine . keyToFreq) [40..50]
-- where pause = renderSignal 0.0 0.05 sampleRate silence
     let 
     play exampleSong 0 bell
