module Images where

import           Data.Char
import           Data.Maybe
import           Graphics.Vty
import qualified Control.Applicative as A

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import           App
import           Sequencer


rootImage :: App -> IO Image
rootImage app = fmap (<-> renderPianoRoll) $ liftM2 (<->) (renderPatternEdit app) (renderHelp app)

renderPatternEdit :: App -> IO Image
renderPatternEdit app = do len <- liftM (length . track) $ readSong app
                           foldr1 (liftM2 horizJoin) $ renderIndex app : [ renderChannel i app | i <- [0 .. (len - 1)]]

renderPianoRoll :: Image
renderPianoRoll = string defAttr "  s d   g h j     2 3   5 6 7  " <->
                  string defAttr " z x c v b n m   q w e r t y u "

renderHelp :: App -> IO Image
renderHelp app = do
    p <- readPlaying app
    return $ string defAttr
        ( "<space>: edit on/off | F1,F2,..: select octave | <return>: "
        ++ (if p then "pause" else "play ")
        ++ " | <esc>: quit | arrows: navigate"
        )

renderChannel :: Int -> App -> IO Image
renderChannel chn app = do
    len <- liftM (length . (!! chn) . track ) $ readSong app
    t <- foldr1 (liftM2 vertJoin) [ renderCell chn i app | i <- [0 .. (len - 1)]]
    return $ pad 1 0 1 0 t

renderIndex :: App -> IO Image
renderIndex app = do
    b <- readPlaying app
    c <- liftM position (readTVarIO (sequencer app))
    len <- liftM (length . (!! 0) . track) $ readSong app
    let row i = string (if b && (i == c) then defAttr `withStyle` reverseVideo else defAttr `withForeColor` brightYellow)
                       (replicate (4 - length (show i)) '0' ++ show i)
    return $ pad 0 0 2 0 $ foldr1 (<->) [ row i | i <- [0 .. (len - 1)] ]

renderCell :: Int -> Int -> App -> IO Image
renderCell chn i app = do
   cell <- liftM ((!! i) . (!! chn) . track) $ readSong app
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
    nth n k      = if cursorX app == n && cursorY app == i && sChannel app == chn
                   then selected (colored k) else colored k
    print1st a   = if isNothing a then "." else [ intToDigit $ fromJust a `mod` 16 ]
    print16th a  = if isNothing a then "." else [ intToDigit $ fromJust a `div` 16 ]
    printPitch c = maybe "..." show (pitch c)
    selected x   = if editMode app
                   then x `withBackColor` red
                   else x `withStyle` reverseVideo

