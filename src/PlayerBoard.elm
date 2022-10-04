module PlayerBoard exposing (..)

import Debug exposing (toString)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Resources exposing (Resources)
import Tiles exposing (Action, RoomTile, TileStatus(..), viewTile)
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


type Subphase msg
    = ChooseRoomToEscavate
    | ChooseSecondRoomToEscavate
    | Furnish
    | PlaceRoom (RoomTile Resources msg)
    | BuildWall
    | EscavateThroughWall
    | DestroyWall
    | Activate1
    | Activate2
    | Activate3


viewBoard : PlayerBoard msg -> Maybe (Subphase msg) -> (RoomTile Resources msg -> msg) -> Html msg
viewBoard board subphase select =
    div [ class "playerboard" ]
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


viewRooms : Resources -> List (RoomTile Resources msg) -> Maybe (Subphase msg) -> (RoomTile Resources msg -> msg) -> List (Html msg)
viewRooms resources rooms subphase select =
    List.indexedMap (viewRoom resources subphase select) rooms


viewRoom : Resources -> Maybe (Subphase msg) -> (RoomTile Resources msg -> msg) -> Int -> RoomTile Resources msg -> Html msg
viewRoom resources subphase select index tile =
    case subphase of
        Just ChooseRoomToEscavate ->
            if tile.status == Rock then
                viewSelectableTile resources select index tile
            else
                viewNonSelectableTile resources index tile

        Just ChooseSecondRoomToEscavate ->
            if tile.status == Rock then
                viewSelectableTile resources select index tile
            else
                viewNonSelectableTile resources index tile

        Just (PlaceRoom t) ->
            if tile.status == Empty then
                viewSelectableTile resources select index tile
            else
                viewNonSelectableTile resources index tile

        Just Activate1 ->
            if tile.status == Available then
                viewSelectableTile resources select index tile
            else
                viewNonSelectableTile resources index tile

        Just Activate2 ->
            if tile.status == Available then
                viewSelectableTile resources select index tile
            else
                viewNonSelectableTile resources index tile

        Just Activate3 ->
            if tile.status == Available then
                viewSelectableTile resources select index tile
            else
                viewNonSelectableTile resources index tile

        _ ->
            viewNonSelectableTile resources index tile


viewSelectableTile resources select index tile =
    div [ class ("room room-" ++ toString index) ]
        [ viewTile [ class "pick", onClick (select tile) ] resources tile ]

viewNonSelectableTile resources index tile =
    div [ class ("room room-" ++ toString index) ]
        [ viewTile [] resources tile ]

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
