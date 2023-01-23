module PlayerBoard exposing (..)

import Array exposing (Array)
import Debug exposing (toString)
import Game exposing (Game, GameMsg(..), PlayerBoard, Resources, Subphase(..), Tile, TileStatus(..), TileType(..), Wall(..))
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Resources
import Tiles exposing (consumeAction, tileCaveEntrance, tileDungeon, tileEmpty, tileEquipmentRoom, tileFreeAction, tileRettingRoom, tileRock, tileWoodStoreroom, viewTile)
import Walls


newBoard : PlayerBoard
newBoard =
    PlayerBoard (Resources 1 1 1 1 1 1 1 7 0) tileFreeAction [] (Array.repeat 14 Game.None) [] Nothing


emptyBoard : PlayerBoard
emptyBoard =
    PlayerBoard (Resources 0 0 0 0 0 0 0 7 0) tileFreeAction [] (Array.fromList []) [] Nothing


init : List Tile -> List Tile
init rooms =
    List.take 4 rooms ++ [ tileEmpty ] ++ (rooms |> List.drop 4 |> List.take 1) ++ [ tileCaveEntrance ] ++ List.drop 5 rooms


restorePlayerPass : PlayerBoard -> PlayerBoard
restorePlayerPass board =
    { board
        | actionTiles = List.map (\t -> { t | status = Available }) board.actionTiles
        , freeAction = Tiles.setStatus Available board.freeAction
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


buildWall : Int -> PlayerBoard -> PlayerBoard
buildWall wallIndex player =
    let
        walls =
            Array.set wallIndex Placed player.walls
    in
    { player
        | walls = walls
        , rooms = Tiles.updateWalls walls player.rooms
        , subphase = Nothing
    }


destroyWall : Int -> PlayerBoard -> PlayerBoard
destroyWall wallIndex player =
    let
        walls =
            Array.set wallIndex Game.None player.walls
    in
    { player
        | walls = walls
        , rooms = Tiles.updateWalls walls player.rooms
        , subphase = Nothing
    }


placeRoom : Tile -> Tile -> PlayerBoard -> PlayerBoard
placeRoom tile tileToPlace player =
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
        && moreOrangeRooms player tile


moreOrangeRooms : PlayerBoard -> Tile -> Bool
moreOrangeRooms player tile =
    let
        orangeCount =
            player.rooms |> List.filter (\t -> t.tileType == Orange) |> List.length

        blueCount =
            player.rooms |> List.filter (\t -> t.tileType == Blue) |> List.length
    in
    if tile.tileType == Blue then
        orangeCount > (blueCount + 1)

    else
        True


playerHasEquipment : PlayerBoard -> Tile -> Bool
playerHasEquipment player tile =
    player.rooms
        |> List.filter (\t -> t.status == Available)
        |> List.filter (\t -> t.title == tile.title)
        |> List.length
        |> (<) 0


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


deactivateRooms player =
    { player
        | subphase = Nothing
        , rooms = Tiles.deactivateTiles player.rooms
    }


activateRoom tile subphase player =
    { player
        | subphase = subphase
        , rooms = Tiles.updateStatus tile Game.Active player.rooms
    }


applyEquipmentRoom subphase player =
    if playerHasEquipment player tileEquipmentRoom then
        case subphase of
            Activate2 first ->
                if first then
                    Just (Activate2 False)

                else
                    Just (Activate1 False)

            Activate3 first ->
                if first then
                    Just (Activate3 False)

                else
                    Just (Activate2 False)

            _ ->
                Nothing

    else
        case subphase of
            Activate2 first ->
                Just (Activate1 False)

            Activate3 first ->
                Just (Activate2 False)

            _ ->
                Nothing


applyRettingRoom player newResources =
    let
        flaxGain =
            newResources.flax - player.resources.flax
    in
    if
        (flaxGain >= 1 && flaxGain <= 3)
            && playerHasEquipment player tileRettingRoom
    then
        Resources.addFood 1 newResources

    else
        newResources


applyDungeon player =
    if playerHasEquipment player tileDungeon then
        { player | resources = Resources.addGold 2 player.resources }

    else
        player


applyWoodStoreroom first qty player =
    if first && playerHasEquipment player tileWoodStoreroom then
        { player | resources = Resources.addFood qty player.resources }

    else
        player


escavateRoom : Tile -> Maybe Subphase -> PlayerBoard -> PlayerBoard
escavateRoom tile subphase player =
    { player
        | subphase = subphase
        , resources = addFoodForBonusRooms player tile
        , rooms =
            Tiles.updateStatus tile Game.Empty player.rooms
                |> Tiles.updateWalls player.walls
    }


addFoodForBonusRooms : PlayerBoard -> Tile -> Resources
addFoodForBonusRooms ({ resources } as player) tile =
    let
        roomIndex =
            indexOf tile player.rooms
    in
    if roomIndex == 3 || roomIndex == 7 then
        Resources.addFood 1 player.resources

    else
        player.resources


indexOf : Tile -> List Tile -> Int
indexOf tile tiles =
    tiles
        |> List.indexedMap Tuple.pair
        |> List.filter (\tp -> (Tuple.second tp).title == tile.title)
        |> List.head
        |> Maybe.withDefault ( 0, tile )
        |> Tuple.first


isExcavatable : PlayerBoard -> Tile -> Bool
isExcavatable board tile =
    let
        roomArray =
            Array.fromList board.rooms

        tileIndex =
            indexOf tile board.rooms
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


viewBoard : PlayerBoard -> Html GameMsg
viewBoard board =
    div [ class "playerboard" ]
        [ viewActionTiles board.resources board.actionTiles
        , div [ class "board" ]
            ([ viewResources board.resources board.subphase, viewFreeAction board ]
                ++ viewRooms board
                ++ viewWalls board
            )
        ]


viewWalls : PlayerBoard -> List (Html GameMsg)
viewWalls board =
    board.walls
        |> Array.indexedMap (viewWall board.subphase)
        |> Array.toList


viewWall : Maybe Subphase -> Int -> Wall -> Html GameMsg
viewWall subphase index wall =
    case wall of
        Game.Placed ->
            div [ class ("wall placed wall-" ++ toString index) ] []

        Game.Optional ->
            div [ class ("wall placed wall-" ++ toString index) ] []

        Game.None ->
            if subphase == Just DestroyWall || subphase == Just BuildWall then
                div [ class ("wall available wall-" ++ toString index), onClick (SelectWall index) ] []

            else
                div [ class ("wall available wall-" ++ toString index) ] []


viewFreeAction : PlayerBoard -> Html GameMsg
viewFreeAction board =
    viewTile [ class "freeaction" ] board.resources board.freeAction


viewActionTiles : Resources -> List Tile -> Html GameMsg
viewActionTiles resources actionTiles =
    div [ class "actiontiles" ] (List.map (viewTile [ class "actiontile" ] resources) actionTiles)


viewRooms : PlayerBoard -> List (Html GameMsg)
viewRooms board =
    List.indexedMap (viewRoom board) board.rooms


viewRoom : PlayerBoard -> Int -> Tile -> Html GameMsg
viewRoom board index tile =
    case board.subphase of
        Just ExcavateThroughWall ->
            if tile.status == Rock && isExcavatable board tile then
                viewSelectableTile board.resources index tile

            else
                viewNonSelectableTile board.resources index tile

        Just Excavate1 ->
            if tile.status == Rock && isExcavatable board tile then
                viewSelectableTile board.resources index tile

            else
                viewNonSelectableTile board.resources index tile

        Just Excavate2 ->
            if tile.status == Rock then
                viewSelectableTile board.resources index tile

            else
                viewNonSelectableTile board.resources index tile

        Just (PlaceRoom t) ->
            if Debug.log "(tile.status == Empty)" (tile.status == Empty) && Walls.matches (Debug.log "(t.walls)" t.walls) (Debug.log "(tile.walls)" tile.walls) then
                viewSelectableTile board.resources index tile

            else
                viewNonSelectableTile board.resources index tile

        Just (Activate1 first) ->
            if tile.status == Available then
                viewSelectableTile board.resources index tile

            else
                viewNonSelectableTile board.resources index tile

        Just (Activate2 first) ->
            if tile.status == Available then
                viewSelectableTile board.resources index tile

            else
                viewNonSelectableTile board.resources index tile

        Just (Activate3 first) ->
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


viewResources resources subphase =
    div [ class "resources" ]
        [ viewResource "food" resources.food subphase (\r -> { r | food = r.food - 1 })
        , viewResource "wood" resources.wood subphase (\r -> { r | wood = r.wood - 1 })
        , viewResource "stone" resources.stone subphase (\r -> { r | stone = r.stone - 1 })
        , viewResource "emmer" resources.emmer subphase (\r -> { r | emmer = r.emmer - 1 })
        , viewResource "flax" resources.flax subphase (\r -> { r | flax = r.flax - 1 })
        , viewResource "gold" resources.gold subphase (\r -> { r | gold = r.gold - 1 })
        ]


viewResource resource qty subphase resfun =
    case subphase of
        Just ChooseResource3 ->
            div [ class (resource ++ " " ++ "qty" ++ toString qty), onClick (ResourceChosen (Just ChooseResource2) resfun) ] []

        Just ChooseResource2 ->
            div [ class (resource ++ " " ++ "qty" ++ toString qty), onClick (ResourceChosen (Just ChooseResource1) resfun) ] []

        Just ChooseResource1 ->
            div [ class (resource ++ " " ++ "qty" ++ toString qty), onClick (ResourceChosen Nothing resfun) ] []

        _ ->
            div [ class (resource ++ " " ++ "qty" ++ toString qty) ] []
