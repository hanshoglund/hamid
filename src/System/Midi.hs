
{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}


--
-- Module      : System.Midi
-- Version     : 0.1
-- License     : BSD3
-- Author      : Balazs Komuves
-- Maintainer  : bkomuves+hmidi@gmail.com
-- Stability   : experimental
-- Portability : not portable
-- Tested with : GHC 6.8.2
--

-- | A lowest common denominator interface to the Win32 and MacOSX Midi bindings. 
-- Error handling is via `fail`-s in the IO monad. 

module System.Midi (
        -- * Messages
        MidiTime,
        MidiMessage,
        MidiEvent,        

        -- * Input and output
        MidiHasName(..),

        -- ** Sources and Destinations
        Source,
        sources,
        Destination,
        destinations,
        
        -- ** Streams
        openSource,
        openDestination,
        Stream,
        close,
        start,
        stop,

        -- * Sending
        send,
        -- sendSysEx,

        -- * Receiving
        getNextEvent,
        getEvents,

        -- * Timer
        currentTime,    
  ) where

import Data.Word (Word8,Word32)
import System.Midi.Base hiding (MidiEvent, MidiMessage)
import System.IO.Unsafe (unsafePerformIO)

import qualified Codec.Midi as C

#ifdef mingw32_HOST_OS
import qualified System.Midi.Win32 as S
#define HMidi_SUPPORTED_OS
#endif

#ifdef darwin_HOST_OS
import qualified System.Midi.MacOSX as S
#define HMidi_SUPPORTED_OS
#endif

-- this is just to be able to produce a Haddock documentation on a not supported system (eg. Linux)
#ifndef HMidi_SUPPORTED_OS
import qualified System.Midi.Placeholder as S
#endif

type MidiTime       = Word32
type MidiMessage    = C.Message
type MidiEvent      = (MidiTime, C.Message)

class MidiHasName a where
    name :: a -> IO String

instance MidiHasName Source where
    name = S.getName . getSource

instance MidiHasName Destination where
    name = S.getName . getDestination
    

-- All the definitions in this file are neccessary to be able to have a nice Haddock-generated
-- documentation independently of the platform. Though I still don't know how to generate documentation
-- for a platform-specific module while being on an a different platform (probably not at all possible 
-- at present?) 

-- | The opaque data type representing a Midi source.
newtype Source = Source { getSource :: S.Source }
    deriving (Eq)

-- | The opaque data type representing a Midi destination.
newtype Destination = Destination { getDestination :: S.Destination }
    deriving (Eq)

instance Show Source where
    show = (\n -> "<Source: "++n++">") . unsafePerformIO . name

instance Show Destination where
    show = (\n -> "<Destination: "++n++">") . unsafePerformIO . name

-- | The opaque data type representing a Midi connection.
newtype Stream = Stream { getStream :: S.Connection }

-- | Enumerates the Midi sources present in the system.
sources :: IO [Source]
sources = fmap (fmap Source) S.enumerateSources

-- | Enumerates the Midi destinations present in the system.
destinations :: IO [Destination]
destinations = fmap (fmap Destination) S.enumerateDestinations

-- | These functions return the name, model and manufacturer of a Midi source \/ destination.
-- 
-- Note: On Win32, only `getName` returns a somewhat meaningful string at the moment.
getName :: S.MidiHasName a => a -> IO String
getModel :: S.MidiHasName a => a -> IO String
getManufacturer :: S.MidiHasName a => a -> IO String

getName = S.getName
getModel = S.getModel
getManufacturer = S.getManufacturer

-- | Opens a Midi Source.
-- There are two possibilites to receive Midi messages. The user can either support a callback function,
-- or get the messages from an asynchronous buffer. However, mixing the two approaches is not allowed.

openSource :: Source -> Maybe (MidiTime -> C.Message -> IO ()) -> IO Stream 
openSource s cb = fmap Stream $ S.openSource (getSource s) (fmap mkCb cb)
    where
        mkCb f (S.MidiEvent ts msg) = f ts (expMsg msg)


-- | Opens a Midi Destination.
openDestination :: Destination -> IO Stream 
openDestination = fmap Stream . S.openDestination . getDestination


-- | Gets the next event from a buffered connection (see also `openSource`)
getNextEvent :: Stream -> IO (Maybe MidiEvent)
getNextEvent = fmap (fmap g) . S.getNextEvent . getStream
    where
        g (S.MidiEvent ts msg) = (ts, expMsg msg)

-- | Gets all the events from the buffer (see also `openSource`)
getEvents :: Stream -> IO [MidiEvent]
getEvents = fmap (fmap g) . S.getEvents . getStream
    where
        g (S.MidiEvent ts msg) = (ts, expMsg msg)
        
-- | Sends a short message. The connection must be a `Destination`.
send :: Stream -> C.Message -> IO ()
send c = S.send (getStream c) . impMsg
 
{-
-- | Sends a system exclusive message. You shouldn't include the starting \/ trailing bytes 0xF0 and 0xF7.
-- 
-- Note: On Win32, the connection must be a `Destination`
sendSysEx :: Stream -> [Word8] -> IO ()
sendSysEx = S.sendSysEx
-}
 
-- | Starts a connection. This is required for receiving Midi messages, and also for starting the clock.
start :: Stream -> IO ()
start = S.start . getStream

-- | Stops a connection.
stop :: Stream -> IO ()
stop = S.stop . getStream
    
-- | Closes a Midi Stream.
close :: Stream -> IO ()
close = S.close . getStream
 
-- | Returns the time elapsed since the last `start` call, in milisecs.
currentTime :: Stream -> IO MidiTime
currentTime = S.currentTime . getStream



impMsg :: C.Message -> S.MidiMessage
impMsg (C.NoteOff ch k _)       = S.MidiMessage ch (S.NoteOff k)
impMsg (C.NoteOn  ch k v)       = S.MidiMessage ch (S.NoteOn k v) 
impMsg (C.ControlChange ch c v) = S.MidiMessage ch (S.CC c v)
impMsg (C.ProgramChange ch a)   = S.MidiMessage ch (S.ProgramChange a)
impMsg (C.PitchWheel ch a)      = S.MidiMessage ch (S.PitchWheel a)

expMsg :: S.MidiMessage -> C.Message
expMsg (S.MidiMessage ch (S.NoteOff k)          ) = C.NoteOff ch k 0
expMsg (S.MidiMessage ch (S.NoteOn k v)	        ) = C.NoteOn  ch k v
expMsg (S.MidiMessage ch (S.CC c v)	            ) = C.ControlChange ch c v
expMsg (S.MidiMessage ch (S.ProgramChange a)	) = C.ProgramChange ch a
expMsg (S.MidiMessage ch (S.PitchWheel a)	    ) = C.PitchWheel ch a
-- expMsg (S.MidiMessage ch (S.PolyAftertouch k v) ) = undefined
-- expMsg (S.MidiMessage ch (S.Aftertouch a)        ) = undefined
-- expMsg (S.SysEx [Word8]                    ) = undefined
-- expMsg (S.SongPosition p                   ) = undefined
-- expMsg (S.SongSelect s                         ) = undefined
-- expMsg (S.TuneRequest                          ) = undefined
-- expMsg (S.SRTClock                             ) = undefined
-- expMsg (S.SRTStart                             ) = undefined
-- expMsg (S.SRTContinue                          ) = undefined
-- expMsg (S.SRTStop                              ) = undefined
-- expMsg (S.ActiveSensing                        ) = undefined
-- expMsg (S.Reset                                ) = undefined
-- expMsg (S.Undefined                           ) = undefined













 
