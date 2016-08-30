module Main where

import           Data.Maybe
import           Data.Char
import           Graphics.Vty

import           State
import           Images

import MidiOutput
import Sequencer
import Control.Monad.IO.Class (liftIO, )
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Debug.Trace

main = do
    listPorts
    putStr "Select port: "
    port <- getLine
    config <- standardIOConfig
    vty <- mkVty config
    withMidiOutput port $ \m -> withState vty m exampleSong main'

main' state = do
    r <- rootImage state
    update (vty state) (picForImage r)
    seq <- readTVarIO $ sequencer state
    sng <- readSong state
    ev <- nextEvent (vty state)
    let numChn = length (track sng) - 1
        mvRchn = if chn < numChn then chn + 1 else 0
        mvLchn = if chn > 0 then chn - 1 else numChn
        len    = length $ track sng !! 0
        
    case ev of
        EvKey KEsc         [] -> shutdown (vty state)
        EvKey KUp          [] -> main' state
                                 { cursorY = if y > 0 then y - 1 else len - 1 }
        EvKey KDown        [] -> main' state
                                 { cursorY = if y < len - 1 then y + 1 else 0 }
        EvKey KRight       [] -> main' state
                                 { cursorX = if x < 7 then x + 1 else 0
                                 , sChannel = if x >= 7 then mvRchn else chn
                                 }
        EvKey KRight  [MCtrl] -> main' state
                                 { sChannel = mvRchn }
        EvKey KLeft        [] -> main' state
                                 { cursorX = if x > 0 then x - 1 else 7
                                 , sChannel = if x <= 0 then mvLchn else chn
                                 }
        EvKey KLeft   [MCtrl] -> main' state
                                 { sChannel = mvLchn }
        EvKey (KFun i)     [] -> main' state { sOctave = i - 1 }
        EvKey (KChar ' ')  [] -> main' state { editMode = not (editMode state) }
        EvKey (KChar c)    [] -> handleKChar state c >> main' state
        EvKey KEnter       [] -> play (sequencer state) >> main' state
        _                     -> main' state
  where x        = cursorX state
        y        = cursorY state
        chn      = sChannel state

-- TODO: support different layouts
--       (maybe query xkb?)
pianoRoll :: [(Char, Pitch)]
pianoRoll = [ ('z', Pitch Cn 0), ('s', Pitch C' 0), ('x', Pitch Dn 0)
            , ('d', Pitch D' 0), ('c', Pitch En 0), ('v', Pitch Fn 0)
            , ('g', Pitch F' 0), ('b', Pitch Gn 0), ('h', Pitch G' 0)
            , ('n', Pitch An 0), ('j', Pitch A' 0), ('m', Pitch Bn 0)
            , (',', Pitch Cn 1), ('l', Pitch C' 1), ('.', Pitch Dn 1)
            , (':', Pitch D' 1), ('q', Pitch Cn 1), ('2', Pitch C' 1)
            , ('w', Pitch Dn 1), ('3', Pitch D' 1), ('e', Pitch En 1)
            , ('r', Pitch Fn 1), ('5', Pitch F' 1), ('t', Pitch Gn 1)
            , ('6', Pitch G' 1), ('y', Pitch An 1), ('7', Pitch A' 1)
            , ('u', Pitch Bn 1), ('i', Pitch Cn 2), ('9', Pitch C' 2)
            , ('o', Pitch Dn 1), ('0', Pitch D' 1)
            ]

handleKChar :: State -> Char -> IO ()
handleKChar s c = when (editMode s) $
    case cursorX s of
        0 -> case lookup c pianoRoll of
                 Just x -> modifyCell (sequencer s)
                             (\a -> a{ pitch = Just $ x { octave = octave x + sOctave s}})
                             (sChannel s)
                             (cursorY s)
                 Nothing -> return ()
        1 -> hexEdit
               (\ a -> a { instrument = Just $ set16th (fromMaybe 0 (instrument a))})
        2 -> hexEdit
               (\a -> a { instrument = Just $ set1st (fromMaybe 0 (instrument a))})
        3 -> hexEdit
               (\a -> a { volpan = Just $ set16th (fromMaybe 0 (volpan a))})
        4 -> hexEdit
               (\a -> a { volpan = Just $ set1st (fromMaybe 0 (volpan a))})
        5 -> hexEdit (\a -> a { fxtype = Just $ digitToInt c})
        6 -> hexEdit
               (\a -> a { fxparam = Just $ set16th (fromMaybe 0 (fxparam a))})
        7 -> hexEdit
               (\a -> a { fxparam = Just $ set1st (fromMaybe 0 (fxparam a))})
        _ -> return ()

  where
    hexEdit f = when (isHexDigit c) $ modifyCell (sequencer s) f (sChannel s) (cursorY s)
    set1st n = n - (n `mod` 16) + digitToInt c
    set16th n = digitToInt c * 16 + (n `mod` 16)


