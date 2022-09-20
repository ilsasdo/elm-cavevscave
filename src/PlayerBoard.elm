module PlayerBoard exposing (..)

import Debug exposing (toString)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Resources exposing (Resources)
import Tiles exposing (Msg, RoomTile, viewTile)
import Walls exposing (Walls)

type alias PlayerBoard =
    { resources : Resources
    , rooms: List (RoomTile Resources)  }


type alias Cave state =
    { bonus : Bool
    , tile : RoomTile state
    , walls : Walls
    }


viewBoard : PlayerBoard -> Html Msg
viewBoard board =
    div [class "board"]
        ([ viewResources board.resources ] ++ viewRooms board.resources board.rooms)

viewRooms: Resources -> List (RoomTile Resources) -> List (Html Msg)
viewRooms resources rooms =
    List.indexedMap (viewRoom resources) rooms

viewRoom: Resources -> Int -> RoomTile Resources -> Html Msg
viewRoom resources index room =
    div [class ("room"++(toString index))] [
        Tiles.viewTile resources room
    ]

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
