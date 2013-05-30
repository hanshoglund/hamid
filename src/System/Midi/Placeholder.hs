
-- | This is just to be able to produce a Haddock documentation on a Linux system

module System.MIDI.Placeholder
    ( module System.MIDI.Base

    , Source
    , Destination
    , Connection
 
    , enumerateSources
    , enumerateDestinations
    
    , MIDIHasName
    , getName
    , getModel
    , getManufacturer

    , openSource
    , openDestination
    , close
    , send
    , sendSysEx
    , start
    , stop
    
    , getNextEvent
    , getEvents
    , currentTime
    
    ) where

import System.MIDI.Base

data Source                 = Source      deriving (Eq, Ord, Show)
data Destination            = Destination deriving (Eq, Ord, Show)
data Connection             = Connection  deriving (Eq, Ord, Show)

enumerateSources            = noImpl
enumerateDestinations       = noImpl

class MIDIHasName a where
getName                     = noImpl 
getManufacturer             = noImpl 
getModel                    = noImpl 
openSource                  = noImpl
openDestination             = noImpl
close                       = noImpl
send                        = noImpl
sendSysEx                   = noImpl
start                       = noImpl
stop                        = noImpl

getNextEvent                = noImpl
getEvents                   = noImpl
currentTime                 = noImpl

noImpl = error "Not implemented"

