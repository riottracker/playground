module MidiOutput where

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Queue as Queue
import qualified Sound.ALSA.Sequencer.Time as Time
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Exception as AlsaExc
import qualified Sound.ALSA.Sequencer.Client.Info as ClientInfo
import qualified Sound.ALSA.Sequencer.Port.Info as PortInfo
import qualified Data.EnumSet as EnumSet
import Text.Printf (printf, )
import Control.Monad (liftM, liftM2, liftM5, join, zipWithM_ )
import Control.Monad.IO.Class (liftIO, )
import Control.Monad.Trans.Cont     (ContT(ContT), runContT)
import System.Environment (getArgs, )
import Debug.Trace

import qualified System.Exit as Exit
import qualified System.IO as IO


data MidiOutput = MidiOutput { handle     :: SndSeq.T SndSeq.DuplexMode
                             , connection :: Connect.T
                             , queue      :: Queue.T
                             , address    :: Addr.T
                             }

withMidiOutput :: String -> (MidiOutput -> IO a) -> IO a
withMidiOutput dest f = (`runContT` return) $ do
    handle <- ContT $ SndSeq.withDefault SndSeq.Block
    liftIO $ Client.setName handle "riottracker"
    port <- ContT $
        Port.withSimple handle "out"
            (Port.caps [Port.capRead, Port.capSubsRead, Port.capWrite])
            (Port.types [Port.typeMidiGeneric, Port.typeApplication])
    queue <- ContT $ Queue.with handle
    liftIO $ mkMidiOutput dest handle port queue f

mkMidiOutput :: String -> SndSeq.T SndSeq.DuplexMode -> Port.T -> Queue.T -> (MidiOutput -> IO a) -> IO a
mkMidiOutput dest handle port queue f = do
    client <- Client.getId handle
    let address = Addr.Cons client port
    connection <- Connect.createTo handle port =<< Addr.parse handle dest
    f $ MidiOutput handle connection queue address


listPorts = do
  putStrLn " Port    Client name                      Port name                Caps"
  SndSeq.withDefault SndSeq.Block $ \h ->
    ClientInfo.queryLoop_ (h :: SndSeq.T SndSeq.OutputMode) $ \cinfo -> do
      client <- ClientInfo.getClient cinfo
      PortInfo.queryLoop_ h client $ \pinfo ->
        join $ liftM5 (printf "%3d:%-3d  %-32.32s %-24.24s %s\n")
          (fmap (\(Client.Cons p) -> p) $ PortInfo.getClient pinfo)
          (fmap (\(Port.Cons p) -> p) $ PortInfo.getPort pinfo)
          (ClientInfo.getName cinfo)
          (PortInfo.getName pinfo)
          (do
             caps <- PortInfo.getCapability pinfo
             let disp (cap, char) =
                    if EnumSet.disjoint caps cap then ' ' else char
             return $ map disp [ (Port.capRead, 'r'), (Port.capSubsRead, 'R') 
                               , (Port.capWrite, 'w'), (Port.capSubsWrite, 'W') ])



