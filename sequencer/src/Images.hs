module Images where

import           Data.Char
import           Data.Maybe
import           Graphics.Vty
import qualified Control.Applicative as A
import           Codec.Tracker.XM.Pattern

import           Editor
import           Sequencer


rootImage :: Editor -> Sequencer -> Image
rootImage ed seq = renderPianoRoll <-> renderPatternEdit ed seq <-> renderHelp seq

renderPatternEdit :: Editor -> Sequencer -> Image
renderPatternEdit ed seq = foldr1 (horizJoin) $ (renderIndex ed seq) : [ renderChannel i ed seq | i <- [0 .. ((nChannels ed) - 1)]]

renderPianoRoll :: Image
renderPianoRoll = string defAttr "  s d   g h j     2 3   5 6 7  " <->
                  string defAttr " z x c v b n m   q w e r t y u "

renderHelp :: Sequencer -> Image
renderHelp seq = string defAttr 
        ( "<space>: edit on/off | F1,F2,..: select octave | <return>: "
        ++ (if playing seq then "pause" else "play ")
        ++ " | <esc>: quit | arrows: navigate"
        )

renderChannel :: Int -> Editor -> Sequencer -> Image
renderChannel chn ed seq = pad 1 0 1 0 $ foldr1 (vertJoin) [ renderCell chn i ed seq | i <- [0 .. ((nRows ed) - 1)]]

renderIndex :: Editor -> Sequencer -> Image
renderIndex ed seq = pad 0 0 2 0 $ foldr1 (<->) [ row i | i <- [0 .. ((nRows ed) - 1)] ]
  where row i = string (if p && (i == c) then defAttr `withStyle` reverseVideo else defAttr `withForeColor` brightYellow) (replicate (4 - length (show i)) '0' ++ show i)
        p = playing seq
        c = position seq

renderCell :: Int -> Int -> Editor -> Sequencer -> Image
renderCell chn i ed seq = string (nth 0 brightWhite) (printPitch cell)
                      <|> string defAttr " "
                      <|>   (string  (nth 1 cyan) (print16th (instrument cell))
                      <|>    string  (nth 2 cyan) (print1st (instrument cell)))
                      <|> string defAttr " "           
                      <|>   (string  (nth 3 green) (print16th (volume cell))
                      <|>    string  (nth 4 green) (print1st (volume cell)))
                      <|> string defAttr " "            
                      <|>   (string  (nth 5 yellow) (print1st (effectType cell))
                      <|>    string  (nth 6 magenta) (print16th (effectParam cell))
                      <|>    string  (nth 7 magenta) (print1st (effectParam cell)))
                      <|> string defAttr " "
  where
    cell         = ((!! i) . (!! chn) . track) $ song seq
    colored k    = defAttr `withForeColor` k
    nth n k      = if cursorX ed == n && cursorY ed == i && sChannel ed == chn
                   then selected (colored k) else colored k
    print1st a   = if isNothing a then "." else [ intToDigit . fromIntegral $ fromJust a `mod` 16 ]
    print16th a  = if isNothing a then "." else [ intToDigit . fromIntegral $ fromJust a `div` 16 ]
    printPitch c = maybe "..." show (note c)
    selected x   = if editMode ed
                   then x `withBackColor` red
                   else x `withStyle` reverseVideo

