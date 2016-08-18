module Main where

import           Data.Maybe
import           Data.Char
import           Graphics.Vty

import           State
import           Images

main = do
    config <- standardIOConfig
    vty <- mkVty config
    main' (defaultState vty) { song = exampleSong }

main' state = do
    update (vty state) (picForImage $ rootImage state)
    ev <- nextEvent (vty state)
    case ev of
        EvKey KEsc         [] -> shutdown (vty state)
        EvKey KUp          [] -> main' state
                                 { cursorY = if y > 0 then y - 1 else len - 1 }
        EvKey KDown        [] -> main' state
                                 { cursorY = if y < len - 1 then y + 1 else 0 }
        EvKey KRight       [] -> main' state
                                 { cursorX = if x < 7 then x + 1 else 0
                                 , sChannel = if not (x < 7) then mvRchn else chn
                                 }
        EvKey KRight  [MCtrl] -> main' state
                                 { sChannel = mvRchn }
        EvKey KLeft        [] -> main' state
                                 { cursorX = if x > 0 then x - 1 else 7
                                 , sChannel = if not (x > 0) then mvLchn else chn
                                 }
        EvKey KLeft   [MCtrl] -> main' state
                                 { sChannel = mvLchn }
        EvKey (KFun i)     [] -> main' state { sOctave = i - 1 }
        EvKey (KChar c)    [] -> main' $ handleKChar state c
        _                     -> main' state
  where x      = cursorX state
        y      = cursorY state
        chn    = sChannel state
        len    = length $ (track (song state)) !! 0
        numChn = (length $ track (song state)) - 1
        mvRchn = if chn < numChn then chn + 1 else 0
        mvLchn = if chn > 0 then chn - 1 else numChn


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

handleKChar :: State -> Char -> State
handleKChar s ' ' = s { editMode = not (editMode s) }
handleKChar s c = if editMode s
                  then case cursorX s of
    0 -> case lookup c pianoRoll of
           Just x ->
             updateAtCursor (\a b -> a { pitch = Just b { octave = (octave b) + (sOctave s) }}) x
           Nothing -> s
    1 -> hexEdit (\a b -> a { instrument = Just $ set16th (fromMaybe 0 (instrument a)) b })
    2 -> hexEdit (\a b -> a { instrument = Just $ set1st  (fromMaybe 0 (instrument a)) b })
    3 -> hexEdit (\a b -> a { volpan     = Just $ set16th (fromMaybe 0 (volpan a)) b })
    4 -> hexEdit (\a b -> a { volpan     = Just $ set1st  (fromMaybe 0 (volpan a)) b })
    5 -> hexEdit (\a b -> a { fxtype     = Just b })
    6 -> hexEdit (\a b -> a { fxparam    = Just $ set16th (fromMaybe 0 (fxparam a)) b})
    7 -> hexEdit (\a b -> a { fxparam    = Just $ set1st  (fromMaybe 0 (fxparam a)) b})
    _ -> s
                  else s
  where
    updateAtCursor f x = s { song = (song s) { track = replaceNth (sChannel s) (
        replaceNth (cursorY s) (f (((track $ song s) !! (sChannel s)) !! (cursorY s)) x) ((track (song s)) !! (sChannel s))
                      ) (track (song s))     }}
    hexEdit f = if isHexDigit c then updateAtCursor f $ digitToInt c else s
    set1st n d = n - (n `mod` 16) + d
    set16th n d = d * 16 + (n `mod` 16)
    replaceNth n new (x : xs)
        | n == 0 = new : xs
        | otherwise = x : replaceNth (n - 1) new xs


