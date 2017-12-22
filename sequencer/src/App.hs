module App where

import qualified Sound.RtMidi as Rt

import           Data.Maybe
import           Data.Char
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM
import           Graphics.Vty
import           Codec.Tracker.Common
import           Codec.Tracker.XM.Pattern

import           Editor
import           Images
import           Sequencer

data App = App { editor      :: TVar Editor
               , vty         :: TVar Vty
               }

defaultApp :: Vty -> Rt.Device -> Song -> IO App
defaultApp v m s = do ed <- defaultEditor m s
                      edi <- newTVarIO ed
                      vi <- newTVarIO v
                      return App { editor    = edi
                                 , vty       = vi
                                 }

withApp :: Vty -> Rt.Device -> Song -> (App -> IO a) -> IO a
withApp v m s f = defaultApp v m s >>= f

render :: App -> IO ()
render app = do
    ed <- readTVarIO $ editor app
    seq <- readTVarIO $ sequencer ed
    v <- readTVarIO $ vty app
    update v (picForImage $ rootImage ed seq)
    threadDelay 20

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
        EvKey (KChar c)    [] -> handleKChar ed c
        EvKey KEnter       [] -> play (sequencer ed)
        _                     -> return ()
 where m f = atomically $ modifyTVar (editor app) f

quit :: App -> IO ()
quit app = do
    v <- readTVarIO $ vty app
    ed <- readTVarIO $ editor app
    seq <- readTVarIO $ sequencer ed
    shutdown v
    killThread $ seqThread ed
    Rt.closeOutput $ output seq
    atomically $ modifyTVar (editor app) (\e -> e { quitEditor = True })

handleKChar :: Editor -> Char -> IO ()
handleKChar ed c = when (editMode ed) $ 
    case cursorX ed of
        0 -> case lookup c pianoRoll of
                 Just x -> modifyCell (sequencer ed)
                             (\a -> a { note = Just . Note  $ toEnum (fromEnum x + 12 * sOctave ed)})
                             (sChannel ed)
                             (cursorY ed)
                 Nothing -> return ()
        1 -> hexEdit
               (\ a -> a { instrument = Just $ set16th (fromMaybe 0 (instrument a))})
        2 -> hexEdit
               (\a -> a { instrument = Just $ set1st (fromMaybe 0 (instrument a))})
        3 -> hexEdit
               (\a -> a { volume = Just $ set16th (fromMaybe 0 (volume a))})
        4 -> hexEdit
               (\a -> a { volume = Just $ set1st (fromMaybe 0 (volume a))})
        5 -> hexEdit (\a -> a { effectType = Just . toEnum . digitToInt $ c})
        6 -> hexEdit
               (\a -> a { effectParam = Just $ set16th (fromMaybe 0 (effectParam a))})
        7 -> hexEdit
               (\a -> a { effectParam = Just $ set1st (fromMaybe 0 (effectParam a))})
        _ -> return ()

  where
    hexEdit f = when (isHexDigit c) $ modifyCell (sequencer ed) f (sChannel ed) (cursorY ed)
    set1st m = let n = fromIntegral m in toEnum $  n - (n `mod` 16) + digitToInt c
    set16th m = let n = fromIntegral m in toEnum $ digitToInt c * 16 + (n `mod` 16)

-- TODO: support different layouts
--       (maybe query xkb?)
pianoRoll :: [(Char, Pitch)]
pianoRoll = [ ('z', Pitch C 0), ('s', Pitch Csharp 0), ('x', Pitch D 0)
            , ('d', Pitch Dsharp 0), ('c', Pitch E 0), ('v', Pitch F 0)
            , ('g', Pitch Fsharp 0), ('b', Pitch G 0), ('h', Pitch Gsharp 0)
            , ('n', Pitch A 0), ('j', Pitch Asharp 0), ('m', Pitch B 0)
            , (',', Pitch C 1), ('l', Pitch Csharp 1), ('.', Pitch D 1)
            , (':', Pitch Dsharp 1), ('q', Pitch C 1), ('2', Pitch Csharp 1)
            , ('w', Pitch D 1), ('3', Pitch Dsharp 1), ('e', Pitch E 1)
            , ('r', Pitch F 1), ('5', Pitch Fsharp 1), ('t', Pitch G 1)
            , ('6', Pitch Gsharp 1), ('y', Pitch A 1), ('7', Pitch Asharp 1)
            , ('u', Pitch B 1), ('i', Pitch C 2), ('9', Pitch Csharp 2)
            , ('o', Pitch D 1), ('0', Pitch Dsharp 1)
            ]

