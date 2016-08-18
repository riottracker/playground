module Images where

import           Data.Char
import           Data.Maybe
import           Graphics.Vty
import qualified Control.Applicative as A

import           State


rootImage :: State -> Image
rootImage state = renderPatternEdit state <-> renderHelp <-> renderPianoRoll

renderPatternEdit :: State -> Image
renderPatternEdit state = foldr1 (<|>) (
         (renderIndex state) : [ renderChannel i state | i <- [0 .. (length (track $ song state) - 1)]]
                                     )
 
renderPianoRoll :: Image
renderPianoRoll = string defAttr "  s d   g h j     2 3   5 6 7  " <->
                   string defAttr " z x c v b n m   q w e r t y u "

renderHelp :: Image
renderHelp = string defAttr
    "<space>: edit on/off | F1,F2,..: select octave | <esc>: quit | arrows: navigate"

renderChannel :: Int -> State -> Image
renderChannel chn state = pad 1 0 1 0 $ foldr1 (<->) [ renderCell chn i state | i <- [0 .. (length ((track (song state)) !! 0) - 1)] ]

renderIndex :: State -> Image
renderIndex state = pad 0 0 2 0 $ foldr1 (<->) [ row i | i <- [0 .. (length ((track (song state)) !! 0) - 1)] ]
    where row i = string (defAttr `withForeColor` brightYellow) $ (\x -> replicate (4 - length x) '0' ++ x) $ show i


renderCell :: Int -> Int -> State -> Image
renderCell chn i state =
        string (nth 0 brightWhite) printPitch
    <|> string defAttr " "
    <|>   (string  (nth 1 cyan) (print16th (instrument cell))
    <|>    string  (nth 2 cyan) (print1st (instrument cell)))
    <|> string defAttr " "           
    <|>   (string  (nth 3 green) (print16th (volpan cell))
    <|>    string  (nth 4 green) (print1st (volpan cell)))
    <|> string defAttr " "            
    <|>   (string  (nth 5 yellow) (print1st (fxtype cell))
    <|>    string  (nth 6 magenta) (print16th (fxparam cell))
    <|>    string  (nth 7 magenta) (print1st (fxparam cell)))
    <|> string defAttr " "
  where
    cell        = ((track (song state)) !! chn) !! i
    colored k   = defAttr `withForeColor` k
    nth n k     = if (cursorX state) == n && (cursorY state) == i && (sChannel state) == chn
                  then selected (colored k) else (colored k)
    print1st a  = if isNothing a then "." else [ intToDigit $ (fromJust a) `mod` 16 ]
    print16th a = if isNothing a then "." else [ intToDigit $ (fromJust a) `div` 16 ]
    printPitch  = if isNothing (pitch cell) then "..." else show (fromJust (pitch cell))
    selected x  = if editMode state
                  then x `withBackColor` red
                  else x `withStyle` reverseVideo

