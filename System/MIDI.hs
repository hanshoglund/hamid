
{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}


--
-- Module      : System.MIDI
-- Version     : 0.1
-- License     : BSD3
-- Author      : Balazs Komuves
-- Maintainer  : bkomuves+hmidi@gmail.com
-- Stability   : experimental
-- Portability : not portable
-- Tested with : GHC 6.8.2
--

-- | A lowest common denominator interface to the Win32 and MacOSX MIDI bindings. 
-- Error handling is via `fail`-s in the IO monad. 

module System.MIDI (
        -- module System.MIDI.Base,

        Source,
        Destination,
        Connection,
        
        enumerateSources,
        enumerateDestinations,

        MidiHasName(..),
        openSource,
        openDestination,
        close,
        send,
        -- sendSysEx,

        start,
        stop,
        getNextEvent,
        getEvents,
        currentTime,    
  ) where

import Data.Word (Word8,Word32)
import System.MIDI.Base
import Control.Newtype

#ifdef mingw32_HOST_OS
import qualified System.MIDI.Win32 as S
#define HMIDI_SUPPORTED_OS
#endif

#ifdef darwin_HOST_OS
import qualified System.MIDI.MacOSX as S
#define HMIDI_SUPPORTED_OS
#endif

-- this is just to be able to produce a Haddock documentation on a not supported system (eg. Linux)
#ifndef HMIDI_SUPPORTED_OS
import qualified System.MIDI.Placeholder as S
#endif

class MidiHasName a where
    name :: a -> IO String

instance MidiHasName Source where
    name = undefined
    -- name = S.getName . getSource

instance MidiHasName Destination where
    name = undefined
    -- name = S.getName . getSource
    

-- All the definitions in this file are neccessary to be able to have a nice Haddock-generated
-- documentation independently of the platform. Though I still don't know how to generate documentation
-- for a platform-specific module while being on an a different platform (probably not at all possible 
-- at present?) 

-- | The opaque data type representing a MIDI source.
newtype Source = Source { getSource :: S.Source }
    deriving (Eq, Show)

-- | The opaque data type representing a MIDI destination.
newtype Destination = Destination { getDestination :: S.Destination }
    deriving (Eq, Show)

-- | The opaque data type representing a MIDI connection.
newtype Connection = Connection { getConnection :: S.Connection }

-- | Enumerates the MIDI sources present in the system.
enumerateSources :: IO [Source]
enumerateSources = fmap (fmap Source) S.enumerateSources

-- | Enumerates the MIDI destinations present in the system.
enumerateDestinations :: IO [Destination]
enumerateDestinations = fmap (fmap Destination) S.enumerateDestinations

-- | These functions return the name, model and manufacturer of a MIDI source \/ destination.
-- 
-- Note: On Win32, only `getName` returns a somewhat meaningful string at the moment.
getName :: S.MIDIHasName a => a -> IO String
getModel :: S.MIDIHasName a => a -> IO String
getManufacturer :: S.MIDIHasName a => a -> IO String

getName = S.getName
getModel = S.getModel
getManufacturer = S.getManufacturer

-- | Opens a MIDI Source.
-- There are two possibilites to receive MIDI messages. The user can either support a callback function,
-- or get the messages from an asynchronous buffer. However, mixing the two approaches is not allowed.
openSource :: Source -> Maybe (MidiEvent -> IO ()) -> IO Connection 
openSource s cb = fmap Connection $ S.openSource (getSource s) cb

-- | Opens a MIDI Destination.
openDestination :: Destination -> IO Connection 
openDestination = fmap Connection . S.openDestination . getDestination

-- | Gets the next event from a buffered connection (see also `openSource`)
getNextEvent :: Connection -> IO (Maybe MidiEvent)
getNextEvent = S.getNextEvent . getConnection

-- | Gets all the events from the buffer (see also `openSource`)
getEvents :: Connection -> IO [MidiEvent]
getEvents = S.getEvents . getConnection
        
-- | Sends a short message. The connection must be a `Destination`.
send :: Connection -> MidiMessage -> IO ()
send = S.send . getConnection
 
{-
-- | Sends a system exclusive message. You shouldn't include the starting \/ trailing bytes 0xF0 and 0xF7.
-- 
-- Note: On Win32, the connection must be a `Destination`
sendSysEx :: Connection -> [Word8] -> IO ()
sendSysEx = S.sendSysEx
-}
 
-- | Starts a connection. This is required for receiving MIDI messages, and also for starting the clock.
start :: Connection -> IO ()
start = S.start . getConnection

-- | Stops a connection.
stop :: Connection -> IO ()
stop = S.stop . getConnection
    
-- | Closes a MIDI Connection.
close :: Connection -> IO ()
close = S.close . getConnection
 
-- | Returns the time elapsed since the last `start` call, in milisecs.
currentTime :: Connection -> IO Word32
currentTime = S.currentTime . getConnection
 
