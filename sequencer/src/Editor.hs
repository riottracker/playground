module Editor where

import           Sound.PortMidi

import           Graphics.Vty
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM

import           MidiOutput
import           Sequencer

data Editor = Editor { cursorY    :: Int
                     , cursorX    :: Int
                     , editMode   :: Bool
                     , sChannel   :: Int
                     , sOctave    :: Int
                     , sequencer  :: TVar Sequencer
                     , seqThread  :: ThreadId
                     , edRedraw   :: Bool
                     , nRows      :: Int
                     , nChannels  :: Int
                     , quitEditor :: Bool
                     }

defaultEditor :: PMStream -> Song -> IO Editor
defaultEditor m s = do seq <- newTVarIO (mkSequencer m) { song = s }
                       tid <- forkIO $ forever $ tick seq
                       return Editor { sequencer  = seq
                                     , seqThread  = tid
                                     , nRows      = (length . (!! 0) . track) s
                                     , nChannels  = (length . track) s
                                     , cursorX    = 0
                                     , cursorY    = 0
                                     , editMode   = False
                                     , sChannel   = 0
                                     , sOctave    = 4
                                     , edRedraw   = True
                                     , quitEditor = False
                                     }

jumpRight ed = ed { sChannel = if sChannel ed < (nChannels ed) - 1 then (sChannel ed) + 1 else 0 }
jumpLeft ed = ed { sChannel = if sChannel ed > 0 then (sChannel ed) - 1 else (nChannels ed) - 1 }

moveRight :: Editor -> Editor
moveRight ed = (if cursorX ed >= 7 then jumpRight ed else ed) { cursorX = if (cursorX ed) < 7 then (cursorX ed) + 1 else 0 }

moveLeft :: Editor -> Editor
moveLeft ed = (if cursorX ed < 1 then jumpLeft ed else ed) { cursorX = if (cursorX ed) > 0 then (cursorX ed) - 1 else 7 }

moveUp :: Editor -> Editor 
moveUp ed = ed { cursorY = if (cursorY ed) > 0 then (cursorY ed) - 1 else (nRows ed) - 1 }

moveDown :: Editor -> Editor
moveDown ed = ed { cursorY = if (cursorY ed) < (nRows ed) - 1 then (cursorY ed) + 1 else 0 }

selectOctave :: Int -> Editor -> Editor
selectOctave n ed = ed { sOctave = n }

switchMode :: Editor -> Editor
switchMode ed = ed { editMode = not (editMode ed) }

redrawEd :: TVar Editor -> IO ()
redrawEd e = atomically $ modifyTVar e (\x -> x { edRedraw = True })

