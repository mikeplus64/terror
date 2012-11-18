module Types where

data World

data Model

data Audio

data Event
    = ClientEvent !ClientEvent
    | WorldEvent  !WorldEvent

data ClientEvent
    = Pan    !Int !Int
    | Zoom   !Int
    | Rotate !Int
    | ExitRequest

data WorldEvent
    = MapChange 
    | GroundViewToggle !Int !Int

