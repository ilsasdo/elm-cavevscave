module PlayerBoard exposing (..)

import Array exposing (Array)
import Debug exposing (toString)
import Game exposing (Action, Game, GameMsg(..), PlayerBoard, Resources, Subphase(..), Tile, TileStatus(..), TileType(..), Wall(..))
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Resources
import Tiles exposing (tileCaveEntrance, tileDungeon, tileEmpty, tileEquipmentRoom, tileFreeAction, tileProspectingSite, tileRettingRoom, tileRock, tileSottobosco, tileWoodStoreroom, viewTile)
import Walls


newBoard : Bool -> Int -> PlayerBoard
newBoard active gold =
    PlayerBoard (Resources 1 1 1 1 1 gold 1 7 0) tileFreeAction [] (Array.repeat 14 Game.None) [] active


emptyBoard : PlayerBoard
emptyBoard =
    PlayerBoard (Resources 0 0 0 0 0 0 0 7 0) tileFreeAction [] (Array.fromList []) [] False


init : List Tile -> List Tile
init rooms =
    List.take 4 rooms ++ [ tileEmpty ] ++ (rooms |> List.drop 4 |> List.take 1) ++ [ tileCaveEntrance ] ++ List.drop 5 rooms


activatePlayer opponentsGold player =
    { player
        | freeAction = player.freeAction |> Tiles.restoreTile |> Tiles.setStatus Active
        , resources = Resources.updateOpponentsGold opponentsGold player.resources
    }


caveIsAllFurnished : PlayerBoard -> Bool
caveIsAllFurnished player =
    let
        emptyOrRockRooms =
            player.rooms
                |> List.filter (\r -> r.status == Rock || r.status == Empty)
                |> List.length
    in
    emptyOrRockRooms == 0


restorePlayerPass : PlayerBoard -> PlayerBoard
restorePlayerPass board =
    { board
        | actionTiles = List.map (\t -> { t | status = Available }) board.actionTiles
        , freeAction = Tiles.setStatus Available board.freeAction
        , rooms = List.map Tiles.restoreTile board.rooms
    }


restorePlayerNextRound : Int -> PlayerBoard -> PlayerBoard
restorePlayerNextRound round ({ resources } as player) =
    { player
        | actionTiles = []
        , rooms = List.map Tiles.restoreTile player.rooms
        , resources = { resources | actions = round }
    }


chooseResource : (Resources -> Resources) -> PlayerBoard -> PlayerBoard
chooseResource updateResources player =
    { player | resources = updateResources player.resources }


doAction : Tile -> Action -> PlayerBoard -> PlayerBoard
doAction tile action player =
    let
        consumedTile =
            Tiles.consumeAction tile action
    in
    { player
        | resources = action.do player.resources |> applyRettingRoom player
        , rooms = updateTile consumedTile player.rooms
        , actionTiles = updateTile consumedTile player.actionTiles
    }


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
    }


placeRoom : Tile -> Tile -> PlayerBoard -> PlayerBoard
placeRoom tile tileToPlace player =
    { player
        | resources = payRoom tileToPlace.price player.resources
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


activateRoom tile player =
    { player
        | rooms = Tiles.updateStatus tile Game.Active player.rooms
    }


applyEquipmentRoom : List Subphase -> PlayerBoard -> List Subphase
applyEquipmentRoom subphase player =
    let
        activateCount =
            subphase
                |> List.filter ((==) Activate)
                |> List.length
    in
    if
        ((activateCount == 2) || (activateCount == 3))
            && playerHasEquipment player tileEquipmentRoom
    then
        Activate :: subphase

    else
        subphase


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


selectActionTile tile player =
    { player | actionTiles = List.append player.actionTiles [ { tile | status = Active } ] }


addAdditionalCave: Tile -> PlayerBoard -> PlayerBoard
addAdditionalCave tile player =
    { player | rooms = List.append player.rooms [{tile | status = Empty }]}


applyProspectingSite : Tile -> PlayerBoard -> PlayerBoard
applyProspectingSite tile player =
    if tile.title == tileSottobosco.title && playerHasEquipment player tileProspectingSite then
        activateRoom tileProspectingSite player

    else
        player


applyDungeon player =
    if playerHasEquipment player tileDungeon then
        { player | resources = Resources.addGold 2 player.resources }

    else
        player


applyWoodStoreroom : List Subphase -> PlayerBoard -> PlayerBoard
applyWoodStoreroom phases player =
    if
        (List.length phases == 1)
            && (List.head phases == Just Activate)
            && playerHasEquipment player tileWoodStoreroom
    then
        { player | resources = Resources.addFood 1 player.resources }

    else
        player


escavateRoom : Tile -> PlayerBoard -> PlayerBoard
escavateRoom tile player =
    { player
        | resources = addFoodForBonusRooms player tile
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


viewBoard : PlayerBoard -> Maybe Subphase -> Html GameMsg
viewBoard board subphase =
    let
        activeClass =
            if board.active then
                "active"

            else
                ""
    in
    div
        [ class "playerboard"
        , class activeClass
        ]
        [ viewActionTiles board.resources board.actionTiles
        , div [ class "board" ]
            ([ viewResources board subphase, viewFreeAction board ]
                ++ viewRooms board subphase
                ++ viewWalls board subphase
            )
        ]


viewWalls : PlayerBoard -> Maybe Subphase -> List (Html GameMsg)
viewWalls board subphase =
    board.walls
        |> Array.indexedMap (viewWall subphase)
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


viewRooms : PlayerBoard -> Maybe Subphase -> List (Html GameMsg)
viewRooms board subphase =
    List.indexedMap (viewRoom board subphase) board.rooms


viewRoom : PlayerBoard -> Maybe Subphase -> Int -> Tile -> Html GameMsg
viewRoom board subphase index tile =
    if not board.active then
        viewNonSelectableTile board.resources index tile

    else
        case subphase of
            Just ExcavateThroughWall ->
                if tile.status == Rock && isExcavatable board tile then
                    viewSelectableTile board.resources index tile

                else
                    viewNonSelectableTile board.resources index tile

            Just Excavate ->
                if tile.status == Rock && isExcavatable board tile then
                    viewSelectableTile board.resources index tile

                else
                    viewNonSelectableTile board.resources index tile

            Just (PlaceRoom t) ->
                if Debug.log "(tile.status == Empty)" (tile.status == Empty) && Walls.matches (Debug.log "(t.walls)" t.walls) (Debug.log "(tile.walls)" tile.walls) then
                    viewSelectableTile board.resources index tile

                else
                    viewNonSelectableTile board.resources index tile

            Just Activate ->
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


viewResources board subphase =
    div [ class "resources" ]
        [ viewResource board.active "food" board.resources.food subphase (\r -> { r | food = r.food - 1 })
        , viewResource board.active "wood" board.resources.wood subphase (\r -> { r | wood = r.wood - 1 })
        , viewResource board.active "stone" board.resources.stone subphase (\r -> { r | stone = r.stone - 1 })
        , viewResource board.active "emmer" board.resources.emmer subphase (\r -> { r | emmer = r.emmer - 1 })
        , viewResource board.active "flax" board.resources.flax subphase (\r -> { r | flax = r.flax - 1 })
        , viewResource board.active "gold" board.resources.gold subphase (\r -> { r | gold = r.gold - 1 })
        ]


viewResource active resource qty subphase resfun =
    if active then
        case subphase of
            Just ChooseResource ->
                div [ class (resource ++ " " ++ "qty" ++ toString qty), onClick (ResourceChosen resfun) ] []

            _ ->
                div [ class (resource ++ " " ++ "qty" ++ toString qty) ] []

    else
        div [ class (resource ++ " " ++ "qty" ++ toString qty) ] []
