module CaveBoard exposing (..)

import Debug exposing (toString)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Resources exposing (Resources)
import Tiles exposing (RoomTile)
import Walls exposing (Walls)

type alias CaveBoard =
    { resources : Resources
    , rooms: List (RoomTile Resources)  }


type alias Cave state =
    { bonus : Bool
    , tile : RoomTile state
    , walls : Walls
    }


viewBoard : CaveBoard -> Html a
viewBoard board =
    div [class "board"]
        [ viewRooms board.rooms
        , viewResources board.resources ]


viewRooms rooms =
    div [] []


viewResources resources =
    div [class "resources"] [
        viewResource "food" resources.food,
        viewResource "wood" resources.wood,
        viewResource "stone" resources.stone,
        viewResource "emmer" resources.emmer,
        viewResource "flax" resources.flax,
        viewResource "gold" resources.gold
    ]

viewResource resource qty =
    div [class (resource++" "++"qty"++toString qty)] []
