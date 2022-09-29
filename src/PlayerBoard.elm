module PlayerBoard exposing (..)

import Debug exposing (toString)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Resources exposing (Resources)
import Tiles exposing (Msg, RoomTile, viewTile)
import Walls exposing (Wall(..), Walls)


type alias PlayerBoard =
    { resources : Resources
    , rooms : List (RoomTile Resources)
    , walls : List Wall
    , actionTiles : List (RoomTile Resources)
    }


type alias Cave state =
    { bonus : Bool
    , tile : RoomTile state
    , walls : Walls
    }


viewBoard : PlayerBoard -> Html Msg
viewBoard board =
    div []
        [ (viewActionTiles board.resources board.actionTiles)
        , div [ class "board" ]
            ([ viewResources board.resources ]
                ++ viewRooms board.resources board.rooms
                ++ viewWalls board.walls
            )
        ]


viewActionTiles: Resources -> List (RoomTile Resources) -> Html Msg
viewActionTiles resources actionTiles =
    div [ class "actiontiles" ] (List.map (viewActionTile resources) actionTiles)


viewActionTile: Resources -> RoomTile Resources -> Html Msg
viewActionTile resources actionTile =
    div [class "actiontile" ] [Tiles.viewTile resources actionTile]


viewWalls : List Wall -> List (Html Msg)
viewWalls walls =
    List.indexedMap viewWall walls


viewWall : Int -> Wall -> Html Msg
viewWall index wall =
    case wall of
        Placed ->
            div [ class ("wall placed wall-" ++ toString index) ] []

        Optional ->
            div [ class ("wall placed wall-" ++ toString index) ] []

        None ->
            div [ class ("wall available wall-" ++ toString index) ] []


viewRooms : Resources -> List (RoomTile Resources) -> List (Html Msg)
viewRooms resources rooms =
    List.indexedMap (viewRoom resources) rooms


viewRoom : Resources -> Int -> RoomTile Resources -> Html Msg
viewRoom resources index room =
    div [ class ("room room-" ++ toString index) ]
        [ Tiles.viewTile resources room ]


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
