module PlayerBoard exposing (..)

import Debug exposing (toString)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Resources exposing (Resources)
import Tiles exposing (Action, RoomTile, viewTile)
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


type Subphase
    = ChooseRoomToEscavate
    | ChooseSecondRoomToEscavate
    | Activate1
    | Activate2
    | Activate3


viewBoard : PlayerBoard msg -> Maybe Subphase -> (RoomTile Resources msg -> msg) -> Html msg
viewBoard board subphase select =
    div [class "playerboard"]
        [ viewActionTiles board.resources board.actionTiles
        , div [ class "board" ]
            ([ viewResources board.resources ]
                ++ viewRooms board.resources board.rooms subphase select
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


viewRooms : Resources -> List (RoomTile Resources msg) -> Maybe Subphase -> (RoomTile Resources msg -> msg) -> List (Html msg)
viewRooms resources rooms subphase select =
    List.indexedMap (viewRoom resources subphase select) rooms


viewRoom : Resources -> Maybe Subphase -> (RoomTile Resources msg -> msg) -> Int -> RoomTile Resources msg -> Html msg
viewRoom resources subphase select index room =
    case subphase of
        Just something ->
            div [ class ("room room-" ++ toString index) ]
                [ viewTile [class "pick", onClick (select room) ] resources room ]

        Nothing ->
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
