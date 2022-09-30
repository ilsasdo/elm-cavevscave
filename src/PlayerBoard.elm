module PlayerBoard exposing (..)

import Debug exposing (toString)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Resources exposing (Resources)
import Tiles exposing (Action, Msg, RoomTile, viewTile)
import Walls exposing (Wall(..), Walls)


type alias PlayerBoard msg =
    { resources : Resources
    , rooms : List (RoomTile Resources msg)
    , walls : List Wall
    , actionTiles : List (RoomTile Resources msg)
    }


type alias Cave state msg =
    { bonus : Bool
    , tile : RoomTile state msg
    , walls : Walls
    }


viewBoard : PlayerBoard msg -> Html msg
viewBoard board =
    div []
        [ viewActionTiles board.resources board.actionTiles
        , div [ class "board" ]
            ([ viewResources board.resources ]
                ++ viewRooms board.resources board.rooms
                ++ viewWalls board.walls
            )
        ]


viewActionTiles : Resources -> List (RoomTile Resources msg) -> Html msg
viewActionTiles resources actionTiles =
    div [ class "actiontiles" ] (List.map (viewTile [ class "actiontile" ] resources) actionTiles)


viewWalls : List Wall -> List (Html msg)
viewWalls walls =
    List.indexedMap viewWall walls


viewWall : Int -> Wall -> Html msg
viewWall index wall =
    case wall of
        Placed ->
            div [ class ("wall placed wall-" ++ toString index) ] []

        Optional ->
            div [ class ("wall placed wall-" ++ toString index) ] []

        None ->
            div [ class ("wall available wall-" ++ toString index) ] []


viewRooms : Resources -> List (RoomTile Resources msg) -> List (Html msg)
viewRooms resources rooms =
    List.indexedMap (viewRoom resources) rooms


viewRoom : Resources -> Int -> RoomTile Resources msg -> Html msg
viewRoom resources index room =
    div [ class ("room room-" ++ toString index) ]
        [ viewTile [] resources room ]


viewResources resources =
    div [ class "resources" ]
        [ viewResource "food" resources.food
        , viewResource "wood" resources.wood
        , viewResource "stone" resources.stone
        , viewResource "emmer" resources.emmer
        , viewResource "flax" resources.flax
        , viewResource "gold" resources.gold
        ]


viewResource resource qty =
    div [ class (resource ++ " " ++ "qty" ++ toString qty) ] []
