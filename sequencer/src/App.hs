module App where

import qualified Sound.PortMidi as PM

import           Data.Maybe
import           Data.Char
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM
import           Graphics.Vty

import           Editor
import           Images
import           Sequencer
import           MidiOutput

data App = App { editor      :: TVar Editor
               , vty         :: TVar Vty
               }

defaultApp :: Vty -> PM.PMStream -> Song -> IO App
defaultApp v m s = do ed <- defaultEditor m s
                      edi <- newTVarIO ed
                      vi <- newTVarIO v
                      return App { editor    = edi
                                 , vty       = vi
                                 }

withApp :: Vty -> PM.PMStream -> Song -> (App -> IO a) -> IO a
withApp v m s f = defaultApp v m s >>= f

render :: App -> IO ()
render app = do
    ed <- readTVarIO $ editor app
    seq <- readTVarIO $ sequencer ed
    v <- readTVarIO $ vty app
    if (edRedraw ed) || (seqRedraw seq) then
      update v (picForImage $ rootImage ed seq)
    else
      return ()

handleEvents app = do
    v <- readTVarIO $ vty app
    ed <- readTVarIO $ editor app
    ev <- nextEvent v
    case ev of
        EvKey KEsc         [] -> quit app
        EvKey KUp          [] -> m moveUp
        EvKey KDown        [] -> m moveDown
        EvKey KRight       [] -> m moveRight
        EvKey KRight  [MCtrl] -> m jumpRight
        EvKey KLeft        [] -> m moveLeft
        EvKey KLeft   [MCtrl] -> m jumpLeft
        EvKey (KFun i)     [] -> m $ selectOctave i
        EvKey (KChar ' ')  [] -> m switchMode
        EvKey (KChar c)    [] -> handleKChar ed c >> redrawEd (editor app)
        EvKey KEnter       [] -> play (sequencer ed)
 where m f = do atomically $ modifyTVar (editor app) f
                redrawEd (editor app)

quit :: App -> IO ()
quit app = do
    v <- readTVarIO $ vty app
    ed <- readTVarIO $ editor app
    seq <- readTVarIO $ sequencer ed
    shutdown v
    PM.close $ output seq
    killThread $ seqThread ed
    atomically $ modifyTVar (editor app) (\e -> e { quitEditor = True })

handleKChar :: Editor -> Char -> IO ()
handleKChar ed c = when (editMode ed) $ 
    case cursorX ed of
        0 -> case lookup c pianoRoll of
                 Just x -> modifyCell (sequencer ed)
                             (\a -> a{ pitch = Just $ x { octave = octave x + sOctave ed}})
                             (sChannel ed)
                             (cursorY ed)
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
    hexEdit f = when (isHexDigit c) $ modifyCell (sequencer ed) f (sChannel ed) (cursorY ed)
    set1st n = n - (n `mod` 16) + digitToInt c
    set16th n = digitToInt c * 16 + (n `mod` 16)


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

