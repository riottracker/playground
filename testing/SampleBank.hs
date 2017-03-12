module SampleBank (SampleBank(..), LoopMode(..), loadSample) where

import qualified Data.Map as M
import Data.WAVE

import Event
import Instrument
import Stream


data LoopMode = NoLoop | Forward | PingPong

data SampleBank = SampleBank
                { bank     :: M.Map Int Stream
                , position :: Int
                , loopMode :: LoopMode
                , tone     :: Maybe Int
                }

instance Instrument_ SampleBank where
  handleEvent NoteOff sb    = sb { position = 0, tone = Nothing }
  handleEvent (NoteOn n) sb = sb { position = 0, tone = Just n }
  render dur sampleRate sb  = (ins, take numSamples $ drop (position sb) $ loop)
    where numSamples = floor $ (fromIntegral sampleRate) * dur  
          ins        = sb { position = (position sb) + numSamples }
          loop       = case loopMode sb of
            NoLoop   -> stream ++ (repeat 0)
            Forward  -> cycle stream
            PingPong -> cycle (stream ++ (reverse stream)) 
          stream     = case tone sb of
            Nothing -> repeat 0
            Just n  -> case M.lookup n $ bank sb of
              Nothing -> repeat 0
              Just x  -> x

loadSample :: String -> Int -> IO Stream
loadSample fn n = fmap (map (!! n) . waveSamples) $ getWAVEFile fn


