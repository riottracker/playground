module Midi where


data MidiOutput = MidiOutput { handle     :: SndSeq.T SndSeq.DuplexMode
                             , port       :: Port.T
                             , queue      :: Queue.T
                             , connection :: Connect.T
                             , address    :: Address.T
                             }

midiConnect :: IO MidiOutput
midiConnect = handleExceptionCont $ do
    liftIO listPorts
    handle <- ContT $ SndSeq.withDefault SndSeq.Block
    liftIO $ Client.setName h "riottracker"
    port <- ContT $
        Port.withSimple h "out"
            (Port.caps [Port.capRead, Port.capSubsRead, Port.capWrite])
            (Port.types [Port.typeMidiGeneric, Port.typeApplication])
    queue <- ContT $ Queue
    c <- Client.getId h
    let address = Addr.Cons c port
    connection <- parseDestArgs h me =<< getArgs
    return MidiOutput{..}
    
