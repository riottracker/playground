module State where

import           Graphics.Vty


data Pitch = Pitch { tone   :: Tone
                   , octave :: Int
                   }
    deriving Eq

instance Show Pitch where
    show p = show (tone p) ++ show (octave p)


data Tone = Cn | C' | Dn | D' | En | Fn | F' | Gn | G' | An | A' | Bn
    deriving Eq

instance Show Tone where
    show Cn = "C-"
    show C' = "C#"
    show Dn = "D-"
    show D' = "D#"
    show En = "E-"
    show Fn = "F-"
    show F' = "F#"
    show Gn = "G-"
    show G' = "G#"
    show An = "A-"
    show A' = "A#"
    show Bn = "B-"

data Cell = Cell { pitch      :: Maybe Pitch
                 , instrument :: Maybe Int
                 , volpan     :: Maybe Int
                 , fxtype     :: Maybe Int
                 , fxparam    :: Maybe Int
                 }

emptyCell :: Cell
emptyCell = Cell Nothing Nothing Nothing Nothing Nothing

data Song = Song { track :: [[Cell]] }

data State = State { song     :: Song
                   , cursorY  :: Int
                   , cursorX  :: Int
                   , editMode :: Bool
                   , sChannel :: Int
                   , sOctave  :: Int
                   , vty      :: Vty
                   }

defaultState :: Vty -> State
defaultState v = State { song     = emptySong
                       , cursorX  = 0
                       , cursorY  = 0
                       , editMode = False
                       , sChannel = 0
                       , sOctave  = 4
                       , vty      = v
                       }

emptySong :: Song
emptySong = Song { track = replicate 4 (replicate 32 emptyCell) }

exampleSong :: Song
exampleSong = Song [
                     [ Cell (Just (Pitch Cn 4)) (Just 1) Nothing Nothing Nothing
                     , emptyCell
                     , Cell (Just (Pitch Dn 4)) (Just 1) Nothing Nothing Nothing
                     , emptyCell
                     , Cell (Just (Pitch En 4)) (Just 1) Nothing Nothing Nothing
                     , emptyCell
                     , Cell (Just (Pitch Fn 4)) (Just 1) Nothing Nothing Nothing
                     , emptyCell
                     , Cell (Just (Pitch Gn 4)) (Just 1) Nothing Nothing Nothing
                     , emptyCell
                     , emptyCell
                     , emptyCell
                     , Cell (Just (Pitch Gn 4)) (Just 1) Nothing Nothing Nothing
                     , emptyCell
                     , emptyCell
                     , emptyCell
                     ]
                     , replicate 16 emptyCell
                   ]

