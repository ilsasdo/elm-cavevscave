module PlayerBoard exposing (..)

import Debug exposing (toString)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Resources exposing (Resources)
import Tiles exposing (Action, Subphase(..), Tile, TileStatus(..), tileSetStatus, viewTile)
import Walls exposing (Wall(..), Walls)


type alias PlayerBoard =
    { resources : Resources
    , rooms : List Tile
    , walls : List Wall
    , actionTiles : List Tile
    , subphase : Maybe Subphase
    }


type alias Cave =
    { bonus : Bool
    , tile : Tile
    , walls : Walls
    }


update: Tiles.Msg -> PlayerBoard -> (PlayerBoard, Maybe Tile)
update msg player =
    case msg of
        Tiles.DoAction tile action ->
            ({ player
                | resources = action.do player.resources
                , subphase = action.subphase
                , rooms = updateTile tile player.rooms
                , actionTiles = updateTile tile player.actionTiles
            }, Nothing)

        Tiles.ActivateTile subphase tile action ->
            ({ player
                | subphase = subphase
                , resources = action.do player.resources
                , rooms = updateTile tile player.rooms
                , actionTiles = updateTile tile player.actionTiles
            }, Nothing)

        Tiles.SelectRoomTile tile ->
            case player.subphase of
                Just Furnish ->
                    ({ player | subphase = Just (PlaceRoom tile) }, Nothing)

                Just (PlaceRoom tileToPlace) ->
                    (placeRoom player tile tileToPlace, Nothing)

                Just DestroyWall ->
                    (player, Nothing)

                Just BuildWall ->
                    (player, Nothing)

                Just EscavateThroughWall ->
                    (escavateRoom player tile Nothing, Just tile)

                Just Escavate1 ->
                    (escavateRoom player tile Nothing, Just tile)

                Just Escavate2 ->
                    (escavateRoom player tile (Just Escavate1), Just tile)

                Just Activate1 ->
                    (activateRoom player tile Nothing, Nothing)

                Just Activate2 ->
                    (activateRoom player tile (Just Activate1), Nothing)

                Just Activate3 ->
                    (activateRoom player tile (Just Activate2), Nothing)

                Nothing ->
                    (player, Nothing)


updateTile : Tile -> List Tile -> List Tile
updateTile tile tiles =
    List.map
        (\r ->
            if r.title == tile.title then
                tile

            else
                r
        )
        tiles


placeRoom: PlayerBoard -> Tile -> Tile -> PlayerBoard
placeRoom player tile tileToPlace =
    { player
        |  resources = payRoom tileToPlace.price player.resources
         , subphase = Nothing
         , rooms =
            List.map
                (\r ->
                    if r.title == tile.title then
                        tileToPlace

                    else
                        r
                )
                player.rooms
    }

playerCanChooseRoom: PlayerBoard -> Tile -> Bool
playerCanChooseRoom player tile =
    playerHaveResources player tile.price


playerHaveResources: PlayerBoard -> Resources -> Bool
playerHaveResources player price =
    payRoom price player.resources |> allResourcesAvailable

allResourcesAvailable: Resources -> Bool
allResourcesAvailable r =
    r.gold >= 0 && r.food >= 0 && r.wood >= 0 && r.flax >= 0 && r.stone >= 0 && r.emmer >= 0

payRoom: Resources -> Resources -> Resources
payRoom price resources =
    { resources | gold = resources.gold - price.gold
                , food = resources.food - price.food
                , wood = resources.wood - price.wood
                , flax = resources.flax - price.flax
                , stone = resources.stone - price.stone
                , emmer = resources.emmer - price.emmer }

activateRoom player tile subphase =
    { player | subphase = subphase
             , rooms = tileSetStatus tile Tiles.Active player.rooms }


escavateRoom player tile subphase =
    { player | subphase = subphase
             , rooms = tileSetStatus tile Tiles.Empty player.rooms }


viewBoard : PlayerBoard -> Maybe Subphase -> (Tile -> Tiles.Msg) -> Html Tiles.Msg
viewBoard board subphase select =
    div [ class "playerboard" ]
        [ viewActionTiles board.resources board.actionTiles
        , div [ class "board" ]
            ([ viewResources board.resources ]
                ++ viewRooms board.resources board.rooms subphase select
                ++ viewWalls board.walls
            )
        ]


viewActionTiles : Resources -> List Tile -> Html Tiles.Msg
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


viewRooms : Resources -> List Tile -> Maybe Subphase -> (Tile -> Tiles.Msg) -> List (Html Tiles.Msg)
viewRooms resources rooms subphase select =
    List.indexedMap (viewRoom resources subphase select) rooms


viewRoom : Resources -> Maybe Subphase -> (Tile -> Tiles.Msg) -> Int -> Tile -> Html Tiles.Msg
viewRoom resources subphase select index tile =
    case subphase of
        Just Escavate1 ->
            if tile.status == Rock then
                viewSelectableTile resources select index tile

            else
                viewNonSelectableTile resources index tile

        Just Escavate2 ->
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
