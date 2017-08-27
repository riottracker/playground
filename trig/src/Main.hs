module Main where

import Foreign.C

import Sound.RtMidi


progression :: [(Int, CUChar)]
progression = [ (3, 54)
              , (2, 37)
              , (5, 49)
              , (4, 41)
              , (2, 45)
              , (1, 41)
              , (3, 37)
              ]

wait :: Device -> CUChar -> Int -> IO ()
wait d note 0 = return ()
wait d note n = do
  m <- getMessage d
  if length (fst m) > 0 then do
    if head (fst m) >= 144 && head (fst m) < 170 && ((fst m) !! 1 == note) then
      wait d note (n - 1)
    else
      wait d note n
  else
    wait d note n
    
train :: Device -> IO CUChar
train d = f
  where f = do
              m <- getMessage d
              if length (fst m) > 0 then do
                putStrLn $ show m
                if head (fst m) >= 144 && head (fst m) < 170 then
                  return $ (fst m) !! 1
                else
                  train d
              else
                  train d


listPorts :: Device -> IO ()
listPorts d = do
  numPorts <- portCount d
  ports <- mapM (portName d) [0..numPorts-1]
  mapM_ (\t -> putStrLn $ show t) $ zip [0..] ports
  return ()

selectPort :: Device -> IO ()
selectPort d = do
  selection <- getLine
  openPort d (read selection) "trig"
  return ()

play :: Device -> Device -> CUChar -> [(Int, CUChar)] -> IO () 
play inp out note [] = return ()
play inp out note (x:xs) = do
                       putStrLn $ show x
                       sendMessage out [145, (snd x), 127]
                       wait inp note $ fst x
                       sendMessage out [129, (snd x), 127]
                       play inp out note xs

main :: IO ()
main = do
  input <- defaultInput
  listPorts input
  putStrLn "select input: "
  selectPort input

  output <- defaultOutput
  listPorts output
  putStrLn "select output: "
  selectPort output

  note <- train input
  putStrLn $ show note

  sendMessage output [193, 89]
  play input output note $ cycle progression 

  closeInput input
  closeOutput output
  return ()
