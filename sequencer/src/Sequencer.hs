{-# LANGUAGE RecordWildCards #-}

module Sequencer where

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Queue as Queue
import qualified Sound.ALSA.Sequencer.Time as Time
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Sequencer.Connect as Connect
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
    , output :: MidiOutput
    } 

mkSequencer :: MidiOutput -> Sequencer
mkSequencer = Sequencer defaultSong 0 False

defaultSong :: Song
defaultSong = 
    Song
    { track = replicate 4 (replicate 32 emptyCell)
    }

modifySequencer :: TVar Sequencer -> (Sequencer -> Sequencer) -> IO ()
modifySequencer seq f = atomically $ modifyTVar seq f

play :: TVar Sequencer -> IO ()
play seq = 
    modifySequencer
        seq
        (\s -> 
              s
              { playing = not (playing s)
              })

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
    let midi = output sequencer
        conn = connection midi
        h = handle midi
        q = queue midi
        me = address midi
        trk = track (song sequencer)
        ev t e = 
            (Event.forConnection conn e)
            { Event.queue = q
            , Event.time = Time.consAbs $ Time.Tick t
            }
    Queue.control h q Event.QueueStart Nothing
    Queue.control h q (Event.QueueTempo (Event.Tempo 10000000)) Nothing
    fmap (const ()) $
        Event.output
            h
            ((ev 1 $ Event.CustomEv Event.Echo $ Event.Custom 0 0 0)
             { Event.dest = me
             })
    _ <- Event.drainOutput h
    _ <- Event.outputPending h
    let waitForEcho = do
            event <- Event.input h
            case Event.body event of
                Event.CustomEv Event.Echo _d -> 
                    if Event.source event == me
                        then (if playing sequencer
                                  then do
                                      modifySequencer
                                          seq
                                          (\s -> 
                                                s
                                                { position = if position s <
                                                                length
                                                                    (trk !! 0) -
                                                                1
                                                      then position s + 1
                                                      else 0
                                                })
                                      mapM_
                                          (\c -> 
                                                case pitch
                                                         (c !!
                                                          position sequencer) of
                                                    Nothing -> return ()
                                                    Just p -> 
                                                        Event.output
                                                            h
                                                            (ev 0 $
                                                             Event.CtrlEv
                                                                 Event.Controller
                                                                 (Event.Ctrl
                                                                      (Event.Channel
                                                                           0)
                                                                      (Event.Parameter
                                                                           0x7B)
                                                                      (Event.Value
                                                                           0x0))) >>
                                                        Event.output
                                                            h
                                                            (ev 0 $
                                                             Event.NoteEv
                                                                 Event.NoteOn $
                                                             Event.simpleNote
                                                                 (Event.Channel
                                                                      0)
                                                                 (Event.Pitch $
                                                                  midiPitch p)
                                                                 Event.normalVelocity) >>
                                                        return ())
                                          trk
                                      tick seq
                                  else tick seq)
                        else waitForEcho
                _ -> waitForEcho
    waitForEcho
