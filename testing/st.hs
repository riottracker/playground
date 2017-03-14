{-# LANGUAGE RecordWildCards #-}

import Data.Maybe
import Stream
import Play
import Keys
import Synth
import Instrument
import Event
import qualified Data.Map as M
import Foreign.C.Types
import Control.Concurrent
import Sound.RtMidi

sampleRate :: Int
sampleRate = 44100


main = do
  device <- defaultInput
  openPort device 0 "Synthesizer test"
  mainLoop (Instrument bell) device

eventFromMessage :: [CUChar] -> Maybe Event
eventFromMessage [0x90, note, _] = Just $ NoteOn (fromEnum note)
eventFromMessage _               = Nothing

mainLoop :: Instrument -> Device -> IO ()  
mainLoop i d = do
  m <- getMessage d
  let ni = if (length (fst m) > 0) then
        (case eventFromMessage (fst m) of
           Just e  -> handleEvent e i
           Nothing -> i
        )
       else i
  let p = render 0.1 sampleRate ni
  playOnStdOut (snd p)
--  if (length $ fst m) > 0 then do
--    putStrLn $ show m
--    mainLoop (fst p) d
--  else
  threadDelay 200
  mainLoop (fst p) d
