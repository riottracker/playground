module Images where

import           Data.Char
import           Data.Maybe
import           Graphics.Vty
import qualified Control.Applicative as A

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import           State
import           Sequencer


rootImage :: State -> IO Image
rootImage state = fmap (<-> renderPianoRoll) $ liftM2 (<->) (renderPatternEdit state) (renderHelp state)

renderPatternEdit :: State -> IO Image
renderPatternEdit state = do len <- liftM (length . track) $ readSong state
                             foldr1 (liftM2 horizJoin) $ renderIndex state : [ renderChannel i state | i <- [0 .. (len - 1)]]

renderPianoRoll :: Image
renderPianoRoll = string defAttr "  s d   g h j     2 3   5 6 7  " <->
                  string defAttr " z x c v b n m   q w e r t y u "

renderHelp :: State -> IO Image
renderHelp state = do
    p <- readPlaying state
    return $ string defAttr
        ( "<space>: edit on/off | F1,F2,..: select octave | <return>: "
        ++ (if p then "pause" else "play ")
        ++ " | <esc>: quit | arrows: navigate"
        )

renderChannel :: Int -> State -> IO Image
renderChannel chn state = do
    len <- liftM (length . (!! chn) . track ) $ readSong state
    t <- foldr1 (liftM2 vertJoin) [ renderCell chn i state | i <- [0 .. (len - 1)]]
    return $ pad 1 0 1 0 t

renderIndex :: State -> IO Image
renderIndex state = do
    b <- readPlaying state
    c <- liftM position (readTVarIO (sequencer state))
    len <- liftM (length . (!! 0) . track) $ readSong state
    let row i = string (if b && (i == c) then defAttr `withStyle` reverseVideo else defAttr `withForeColor` brightYellow)
                       (replicate (4 - length (show i)) '0' ++ show i)
    return $ pad 0 0 2 0 $ foldr1 (<->) [ row i | i <- [0 .. (len - 1)] ]

renderCell :: Int -> Int -> State -> IO Image
renderCell chn i state = do
   cell <- liftM ((!! i) . (!! chn) . track) $ readSong state
   return $ 
        string (nth 0 brightWhite) (printPitch cell)
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
    colored k    = defAttr `withForeColor` k
    nth n k      = if cursorX state == n && cursorY state == i && sChannel state == chn
                   then selected (colored k) else colored k
    print1st a   = if isNothing a then "." else [ intToDigit $ fromJust a `mod` 16 ]
    print16th a  = if isNothing a then "." else [ intToDigit $ fromJust a `div` 16 ]
    printPitch c = maybe "..." show (pitch c)
    selected x   = if editMode state
                   then x `withBackColor` red
                   else x `withStyle` reverseVideo

