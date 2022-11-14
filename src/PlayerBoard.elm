module PlayerBoard exposing (..)

import Array exposing (Array)
import Debug exposing (toString)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Resources exposing (Resources)
import Tiles exposing (Action, Msg(..), Subphase(..), Tile, TileStatus(..), consumeAction, tileCaveEntrance, tileEmpty, tileRock, viewTile)
import Walls exposing (Wall(..), Walls)


type alias PlayerBoard =
    { resources : Resources
    , rooms : List Tile
    , walls : Array Wall
    , actionTiles : List Tile
    , subphase : Maybe Subphase
    }


type Msg
    = AddToAvailableRooms Tile
    | RemoveFromAvailableRooms Tile
    | WallBuilt
    | WallDestroyed
    | None


restorePlayerPass : PlayerBoard -> PlayerBoard
restorePlayerPass board =
    { board
        | actionTiles = List.map (\t -> { t | status = Available }) board.actionTiles
        , rooms = List.map restoreTile board.rooms
    }


restorePlayerNextRound : PlayerBoard -> Int -> PlayerBoard
restorePlayerNextRound ({ resources } as player) round =
    { player
        | actionTiles = []
        , rooms = List.map restoreTile player.rooms
        , resources = { resources | actions = round }
    }


restoreTile : Tile -> Tile
restoreTile room =
    if room.status == Active then
        { room
            | status = Available
            , actions = List.map (\a -> { a | available = True }) room.actions
        }

    else
        room


init : List Tile -> List Tile
init rooms =
    List.take 4 rooms ++ [ tileEmpty ] ++ (rooms |> List.drop 4 |> List.take 1) ++ [ tileCaveEntrance ] ++ List.drop 5 rooms


update : Tiles.Msg -> PlayerBoard -> ( PlayerBoard, Msg )
update msg player =
    case msg of
        Tiles.DoAction tile action ->
            let
                consumedTile =
                    consumeAction tile action
            in
            ( { player
                | resources = action.do player.resources
                , subphase = action.subphase
                , rooms = updateTile consumedTile player.rooms
                , actionTiles = updateTile consumedTile player.actionTiles
              }
            , None
            )

        Tiles.SelectWall index ->
            case player.subphase of
                Just BuildWall ->
                    ( buildWall player index, WallBuilt )

                Just DestroyWall ->
                    ( destroyWall player index, WallDestroyed )

                _ ->
                    ( player, None )

        Tiles.SelectRoomTile tile ->
            case player.subphase of
                Just Furnish ->
                    ( { player | subphase = Just (PlaceRoom tile) }, None )

                Just (PlaceRoom tileToPlace) ->
                    ( placeRoom player tile tileToPlace, RemoveFromAvailableRooms tileToPlace )

                Just EscavateThroughWall ->
                    ( escavateRoom player tile Nothing, AddToAvailableRooms tile )

                Just Escavate1 ->
                    ( escavateRoom player tile Nothing, AddToAvailableRooms tile )

                Just Escavate2 ->
                    ( escavateRoom player tile (Just Escavate1), AddToAvailableRooms tile )

                Just Activate1 ->
                    ( activateRoom player tile Nothing, None )

                Just Activate2 ->
                    ( activateRoom player tile (Just Activate1), None )

                Just Activate3 ->
                    ( activateRoom player tile (Just Activate2), None )

                _ ->
                    ( player, None )


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


buildWall : PlayerBoard -> Int -> PlayerBoard
buildWall player wallIndex =
    let
        walls =
            Array.set wallIndex Placed player.walls
    in
    { player
        | walls = walls
        , rooms = Tiles.updateWalls walls player.rooms
        , subphase = Nothing
    }


destroyWall : PlayerBoard -> Int -> PlayerBoard
destroyWall player wallIndex =
    let
        walls =
            Array.set wallIndex Walls.None player.walls
    in
    { player
        | walls = walls
        , rooms = Tiles.updateWalls walls player.rooms
        , subphase = Nothing
    }


placeRoom : PlayerBoard -> Tile -> Tile -> PlayerBoard
placeRoom player tile tileToPlace =
    { player
        | resources = payRoom tileToPlace.price player.resources
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


isRoomSelectable : PlayerBoard -> Tile -> Bool
isRoomSelectable player tile =
    playerHaveResources player tile.price
        && playerCanPlaceRoom player tile


playerCanPlaceRoom : PlayerBoard -> Tile -> Bool
playerCanPlaceRoom player tile =
    player.rooms
        |> List.map (\t -> t.status == Empty && Walls.matches t.walls tile.walls)
        |> List.foldl (||) False


playerHaveResources : PlayerBoard -> Resources -> Bool
playerHaveResources player price =
    payRoom price player.resources |> allResourcesAvailable


allResourcesAvailable : Resources -> Bool
allResourcesAvailable r =
    r.gold >= 0 && r.food >= 0 && r.wood >= 0 && r.flax >= 0 && r.stone >= 0 && r.emmer >= 0


payRoom : Resources -> Resources -> Resources
payRoom price resources =
    { resources
        | gold = resources.gold - price.gold
        , food = resources.food - price.food
        , wood = resources.wood - price.wood
        , flax = resources.flax - price.flax
        , stone = resources.stone - price.stone
        , emmer = resources.emmer - price.emmer
    }


activateRoom player tile subphase =
    { player
        | subphase = subphase
        , rooms = Tiles.updateStatus tile Tiles.Active player.rooms
    }


escavateRoom : PlayerBoard -> Tile -> Maybe Subphase -> PlayerBoard
escavateRoom player tile subphase =
    { player
        | subphase = subphase
        , rooms =
            Tiles.updateStatus tile Tiles.Empty player.rooms
                |> Tiles.updateWalls player.walls
    }


isReachableRoom : PlayerBoard -> Tile -> Bool
isReachableRoom board tile =
    let
        roomArray =
            Array.fromList board.rooms

        tileIndex =
            getTileIndex board.rooms tile
    in
    isReachable tileIndex roomArray


isReachable tileIndex roomArray =
    case tileIndex of
        0 ->
            isRoom 1 roomArray
                || isRoom 2 roomArray

        1 ->
            isRoom 0 roomArray
                || isRoom 3 roomArray

        2 ->
            isRoom 0 roomArray
                || isRoom 4 roomArray
                || isRoom 3 roomArray

        3 ->
            isRoom 1 roomArray
                || isRoom 5 roomArray
                || isRoom 2 roomArray

        4 ->
            isRoom 2 roomArray
                || isRoom 5 roomArray
                || isRoom 6 roomArray

        5 ->
            isRoom 3 roomArray
                || isRoom 4 roomArray
                || isRoom 7 roomArray

        6 ->
            isRoom 4 roomArray
                || isRoom 7 roomArray
                || isRoom 8 roomArray

        7 ->
            isRoom 5 roomArray
                || isRoom 6 roomArray
                || isRoom 9 roomArray

        8 ->
            isRoom 9 roomArray
                || isRoom 6 roomArray

        9 ->
            isRoom 7 roomArray
                || isRoom 10 roomArray
                || isRoom 8 roomArray

        10 ->
            isRoom 9 roomArray

        _ ->
            False


isRoom : Int -> Array Tile -> Bool
isRoom tileIndex roomArray =
    Array.get tileIndex roomArray
        |> Maybe.withDefault tileRock
        |> (\t -> t.status /= Rock)


getTileIndex tiles tile =
    tiles
        |> List.indexedMap (\i -> \t -> ( i, t ))
        |> List.filter (\( i, t ) -> tile.title == t.title)
        |> List.head
        |> Maybe.withDefault ( 0, tile )
        |> Tuple.first


viewBoard : PlayerBoard -> Html Tiles.Msg
viewBoard board =
    div [ class "playerboard" ]
        [ viewActionTiles board.resources board.actionTiles
        , div [ class "board" ]
            ([ viewResources board.resources ]
                ++ viewRooms board
                ++ viewWalls board
            )
        ]


viewWalls : PlayerBoard -> List (Html Tiles.Msg)
viewWalls board =
    board.walls
        |> Array.indexedMap (viewWall board.subphase)
        |> Array.toList


viewWall : Maybe Subphase -> Int -> Wall -> Html Tiles.Msg
viewWall subphase index wall =
    case wall of
        Walls.Placed ->
            div [ class ("wall placed wall-" ++ toString index) ] []

        Walls.Optional ->
            div [ class ("wall placed wall-" ++ toString index) ] []

        Walls.None ->
            if subphase == Just DestroyWall || subphase == Just BuildWall then
                div [ class ("wall available wall-" ++ toString index), onClick (SelectWall index) ] []

            else
                div [ class ("wall available wall-" ++ toString index) ] []


viewActionTiles : Resources -> List Tile -> Html Tiles.Msg
viewActionTiles resources actionTiles =
    div [ class "actiontiles" ] (List.map (viewTile [ class "actiontile" ] resources) actionTiles)


viewRooms : PlayerBoard -> List (Html Tiles.Msg)
viewRooms board =
    List.indexedMap (viewRoom board) board.rooms


viewRoom : PlayerBoard -> Int -> Tile -> Html Tiles.Msg
viewRoom board index tile =
    case board.subphase of
        Just Escavate1 ->
            if tile.status == Rock && isReachableRoom board tile then
                viewSelectableTile board.resources index tile

            else
                viewNonSelectableTile board.resources index tile

        Just Escavate2 ->
            if tile.status == Rock then
                viewSelectableTile board.resources index tile

            else
                viewNonSelectableTile board.resources index tile

        Just (PlaceRoom t) ->
            if (Debug.log "(tile.status == Empty)" ((tile.status == Empty))) && Walls.matches (Debug.log "(t.walls)" ((t.walls))) (Debug.log "(tile.walls)" ((tile.walls))) then
                viewSelectableTile board.resources index tile

            else
                viewNonSelectableTile board.resources index tile

        Just Activate1 ->
            if tile.status == Available then
                viewSelectableTile board.resources index tile

            else
                viewNonSelectableTile board.resources index tile

        Just Activate2 ->
            if tile.status == Available then
                viewSelectableTile board.resources index tile

            else
                viewNonSelectableTile board.resources index tile

        Just Activate3 ->
            if tile.status == Available then
                viewSelectableTile board.resources index tile

            else
                viewNonSelectableTile board.resources index tile

        _ ->
            viewNonSelectableTile board.resources index tile


viewSelectableTile resources index tile =
    div [ class ("room room-" ++ toString index) ]
        [ viewTile [ class "pick", onClick (SelectRoomTile tile) ] resources tile ]


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
