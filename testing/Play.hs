module Play (playOnStdOut) where

import Stream
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as B (putStr)

playOnStdOut :: Stream -> IO ()
playOnStdOut s = do B.putStr $ encode s
