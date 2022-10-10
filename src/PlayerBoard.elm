module PlayerBoard exposing (..)

import Array exposing (Array)
import Debug exposing (toString)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Resources exposing (Resources)
import Tiles exposing (Action, Subphase(..), Tile, TileStatus(..), tileCaveEntrance, tileEmpty, updateStatus, viewTile)
import Walls exposing (Wall(..), Walls, viewWalls)


type alias PlayerBoard =
    { resources : Resources
    , rooms : List Tile
    , walls : Array Wall
    , actionTiles : List Tile
    , subphase : Maybe Subphase
    }

type Msg =
    NewTileAvailable Tile
    | WallBuilt
    | WallDestroyed
    | None


init: List Tile -> List Tile
init rooms =
    List.take 4 rooms ++ [ tileEmpty ] ++ (rooms |> List.drop 4 |> List.take 1) ++ [ tileCaveEntrance ] ++ List.drop 5 rooms


update: Tiles.Msg -> PlayerBoard -> (PlayerBoard, Msg)
update msg player =
    case msg of
        Tiles.DoAction tile action ->
            ({ player
                | resources = action.do player.resources
                , subphase = action.subphase
                , rooms = updateTile tile player.rooms
                , actionTiles = updateTile tile player.actionTiles
            }, None)

        Tiles.ActivateTile subphase tile action ->
            ({ player
                | subphase = subphase
                , resources = action.do player.resources
                , rooms = updateTile tile player.rooms
                , actionTiles = updateTile tile player.actionTiles
            }, None)

        Tiles.SelectRoomTile tile ->
            case player.subphase of
                Just Furnish ->
                    ({ player | subphase = Just (PlaceRoom tile) }, None)

                Just (PlaceRoom tileToPlace) ->
                    (placeRoom player tile tileToPlace, None)

                Just BuildWall ->
                    ({ player | subphase = Just (PlaceRoom tile) }, None)

                Just (DestroyWall index) ->
                    (destroyWall player index, WallDestroyed)

                Just (BuildWall index) ->
                    (buildWall player index, WallBuilt)

                Just EscavateThroughWall ->
                    (escavateRoom player tile Nothing, NewTileAvailable tile)

                Just Escavate1 ->
                    (escavateRoom player tile Nothing, NewTileAvailable tile)

                Just Escavate2 ->
                    (escavateRoom player tile (Just Escavate1), NewTileAvailable tile)

                Just Activate1 ->
                    (activateRoom player tile Nothing, None)

                Just Activate2 ->
                    (activateRoom player tile (Just Activate1), None)

                Just Activate3 ->
                    (activateRoom player tile (Just Activate2), None)

                Nothing ->
                    (player, None)


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


buildWall: PlayerBoard -> Int -> PlayerBoard
buildWall player wallIndex =
    let
        walls = Array.set wallIndex Placed player.walls
    in
    { player | walls = walls
             , rooms = Tiles.updateWalls walls player.rooms }


destroyWall: PlayerBoard -> Int -> PlayerBoard
destroyWall player wallIndex =
    let
        walls = Array.set wallIndex Walls.None player.walls
    in
    { player | walls = walls
             , rooms = Tiles.updateWalls walls player.rooms }


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

isRoomSelectable: PlayerBoard -> Tile -> Bool
isRoomSelectable player tile =
    playerHaveResources player tile.price &&
    playerCanPlaceRoom player tile


playerCanPlaceRoom: PlayerBoard -> Tile -> Bool
playerCanPlaceRoom player tile =
    player.rooms
    |> List.filter (\t -> t.status == Empty)
    |> List.map (\t -> Walls.matches t.walls tile.walls)
    |> List.foldl (||) False


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
             , rooms = Tiles.updateStatus tile Tiles.Active player.rooms }


escavateRoom: PlayerBoard -> Tile -> Maybe Subphase -> PlayerBoard
escavateRoom player tile subphase =
    { player | subphase = subphase
             , rooms = Tiles.updateStatus tile Tiles.Empty player.rooms
                       |> Tiles.updateWalls player.walls }


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
