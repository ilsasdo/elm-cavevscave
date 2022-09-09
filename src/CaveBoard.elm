module CaveBoard exposing (..)

import Resources exposing (Resources)
import Tiles exposing (RoomTile)
import Walls exposing (Walls)

type alias CaveBoard =
    { resources : Resources }


type alias Cave state =
    { bonus : Bool
    , tile : RoomTile state
    , walls : Walls
    }