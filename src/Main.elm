module Main exposing (main)

import Browser
import Game exposing (..)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import PlayerBoard exposing (isRoomSelectable, restorePlayerNextRound, restorePlayerPass, viewBoard)
import Stack
import Tiles exposing (tileAdditionalCavern3Walls, tileAdditionalCavern4Walls, viewTile)


main =
    Browser.element { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }


init : () -> ( Game, Cmd GameMsg )
init _ =
    soloPlayerGame


soloPlayerGame =
    ( Game SoloGame (PlayerBoard.newBoard True 1) (PlayerBoard.newBoard False -1) 1 2 [] [] 7 [ NewActionPhase ], Tiles.soloPlayerTiles )


twoPlayersGame =
    ( Game TwoPlayersGame (PlayerBoard.newBoard True 1) (PlayerBoard.newBoard False 1) 1 2 [] Tiles.startingCommonRooms 7 [ NewActionPhase ], Tiles.twoPlayersTiles )


update : GameMsg -> Game -> ( Game, Cmd GameMsg )
update msg ({ player1, player2 } as game) =
    let
        activePlayer =
            getCurrentPlayer game
    in
    case msg of
        InitActionTiles tiles ->
            ( { game
                | actionTiles =
                    List.indexedMap
                        (\i ->
                            \t ->
                                if i < 5 then
                                    { t | status = Available }

                                else
                                    t
                        )
                        (game.actionTiles ++ tiles)
              }
            , Cmd.none
            )

        InitPlayerBoard rooms ->
            ( { game
                | player1 =
                    { player1
                        | rooms = List.take 9 rooms |> PlayerBoard.init
                        , freeAction = Tiles.setStatus Active player1.freeAction
                    }
                , player2 =
                    { player2
                        | rooms = List.drop 9 rooms |> List.take 9 |> PlayerBoard.init
                        , freeAction = Tiles.setStatus Available player1.freeAction
                    }
              }
            , Cmd.none
            )

        InitCommonRooms ( availableRooms, discardedRooms ) ->
            ( { game | availableRooms = availableRooms }, Cmd.none )

        Pass ->
            ( pass game, Cmd.none )

        PickRoundTile tile ->
            game
                |> getCurrentPlayer
                |> PlayerBoard.selectActionTile tile
                |> PlayerBoard.applyProspectingSite tile
                |> setCurrentPlayer game
                |> pushToPhase [ ActionPhase ]
                |> removeActionTile tile
                |> Tuple.pair Cmd.none
                |> swap

        DoAction tile action ->
            game
                |> getCurrentPlayer
                |> PlayerBoard.doAction tile action
                |> PlayerBoard.applyWoodStoreroom action.subphase
                |> setCurrentPlayer game
                |> pushToPhase (PlayerBoard.applyEquipmentRoom action.subphase (getCurrentPlayer game))
                |> Tuple.pair Cmd.none
                |> swap

        SelectWall index ->
            case Stack.top game.stack of
                Just BuildWall ->
                    activePlayer
                        |> PlayerBoard.buildWall index
                        |> PlayerBoard.applyDungeon
                        |> setCurrentPlayer game
                        |> updateAvailableWalls -1
                        |> Tuple.pair Cmd.none
                        |> swap

                Just DestroyWall ->
                    activePlayer
                        |> PlayerBoard.destroyWall index
                        |> setCurrentPlayer game
                        |> updateAvailableWalls 1
                        |> Tuple.pair Cmd.none
                        |> swap

                _ ->
                    ( game, Cmd.none )

        SelectRoomTile tile ->
            case Stack.top game.stack of
                Just Furnish ->
                    game
                        |> popFromPhase
                        |> pushToPhase [ PlaceRoom tile ]
                        |> Tuple.pair Cmd.none
                        |> swap

                Just (PlaceRoom tileToPlace) ->
                    activePlayer
                        |> PlayerBoard.placeRoom tile tileToPlace
                        |> setCurrentPlayer game
                        |> popFromPhase
                        |> removeFromAvailableRooms tileToPlace
                        |> applyAdditionalCave
                        |> Tuple.pair Cmd.none
                        |> swap

                Just ExcavateThroughWall ->
                    activePlayer
                        |> PlayerBoard.escavateRoom tile
                        |> setCurrentPlayer game
                        |> addToAvailableRooms tile
                        |> popFromPhase
                        |> Tuple.pair Cmd.none
                        |> swap

                Just (Excavate times) ->
                    activePlayer
                        |> PlayerBoard.escavateRoom tile
                        |> setCurrentPlayer game
                        |> addToAvailableRooms tile
                        |> soloPlayerUncoverRoom times
                        |> popFromPhase
                        |> Tuple.pair Cmd.none
                        |> swap

                Just Activate ->
                    activePlayer
                        |> PlayerBoard.activateRoom tile
                        |> setCurrentPlayer game
                        |> popFromPhase
                        |> Tuple.pair Cmd.none
                        |> swap

                Just SelectAdditionalCave ->
                    activePlayer
                        |> PlayerBoard.addAdditionalCave tile
                        |> setCurrentPlayer game
                        |> popFromPhase
                        |> Tuple.pair Cmd.none
                        |> swap

                _ ->
                    ( game, Cmd.none )

        ResourceChosen updateResources ->
            game
                |> getCurrentPlayer
                |> PlayerBoard.chooseResource updateResources
                |> setCurrentPlayer game
                |> popFromPhase
                |> Tuple.pair Cmd.none
                |> swap


soloPlayerUncoverRoom : Int -> Game -> Game
soloPlayerUncoverRoom excavateTimes ({ player2 } as game) =
    let
        room =
            List.head game.player2.rooms
    in
    if excavateTimes == 2 then
        game

    else
        case room of
            Just r ->
                { game
                    | availableRooms = game.availableRooms ++ [ { r | status = Available } ]
                    , player2 = { player2 | rooms = List.drop 1 player2.rooms }
                }

            Nothing ->
                game


applyAdditionalCave: Game -> Game
applyAdditionalCave game =
    if
        PlayerBoard.caveIsAllFurnished (getCurrentPlayer game)
            && isAdditionalCaveAvailable game
    then
        pushToPhase [ SelectAdditionalCave ] game

    else
        game


isAdditionalCaveAvailable : Game -> Bool
isAdditionalCaveAvailable game =
    (List.length game.player1.rooms == 10)
        && (List.length game.player2.rooms == 10)


popFromPhase : Game -> Game
popFromPhase game =
    { game | stack = Stack.pop game.stack }


pushToPhase : List Subphase -> Game -> Game
pushToPhase subphase game =
    { game | stack = Stack.pushAll subphase game.stack }


swap ( a, b ) =
    ( b, a )


setCurrentPlayer : Game -> PlayerBoard -> Game
setCurrentPlayer game player =
    if game.player1.active then
        { game | player1 = player }

    else
        { game | player2 = player }


removeActionTile : Tile -> Game -> Game
removeActionTile tile game =
    { game | actionTiles = Tiles.updateStatus tile Empty game.actionTiles }


pass : Game -> Game
pass game =
    if game.gameType == TwoPlayersGame then
        twoPlayersGamePass game

    else
        soloPlayerGamePass game


soloPlayerGamePass game =
    if List.length game.player1.actionTiles == game.actions then
        nextRound game

    else
        game
            |> getCurrentPlayer
            |> restorePlayerPass
            |> setCurrentPlayer { game | stack = [ NewActionPhase ] }
            |> activatePlayer


twoPlayersGamePass game =
    if
        (List.length game.player1.actionTiles == game.actions)
            && (List.length game.player2.actionTiles == game.actions)
    then
        nextRound game

    else
        game
            |> getCurrentPlayer
            |> restorePlayerPass
            |> setCurrentPlayer game
            |> nextPlayer
            |> activatePlayer


activatePlayer : Game -> Game
activatePlayer game =
    let
        opponent =
            opponentPlayer game
    in
    game
        |> getCurrentPlayer
        |> PlayerBoard.activatePlayer opponent.resources.gold
        |> setCurrentPlayer game


updateAvailableWalls : Int -> Game -> Game
updateAvailableWalls qty ({ player1, player2 } as game) =
    { game
        | availableWalls = game.availableWalls + qty
        , player1 = updateAvailableWallsResource qty player1
        , player2 = updateAvailableWallsResource qty player2
    }


updateAvailableWallsResource : Int -> PlayerBoard -> PlayerBoard
updateAvailableWallsResource qty ({ resources } as player) =
    { player | resources = { resources | availableWalls = resources.availableWalls + qty } }


addToAvailableRooms : Tile -> Game -> Game
addToAvailableRooms tile game =
    { game | availableRooms = game.availableRooms ++ [ { tile | status = Available } ] }


removeFromAvailableRooms : Tile -> Game -> Game
removeFromAvailableRooms tile game =
    { game | availableRooms = List.filter (\r -> r.title /= tile.title) game.availableRooms }


nextRound : Game -> Game
nextRound game =
    let
        round =
            game.round + 1

        -- 2 actions for rounds 1,2,3. 3 actions for rounds 4,5,6,7. 4 actions for round 8
        actions =
            if round < 4 then
                2

            else if round < 8 then
                3

            else
                4

        -- restore the action tiles and flip the next round tile
        actionTiles =
            List.indexedMap
                (\i ->
                    \r ->
                        if i <= (round + 3) then
                            { r | status = Available }

                        else
                            r
                )
                game.actionTiles
    in
    { game
        | stack = [ NewActionPhase ]
        , round = round
        , actions = actions
        , actionTiles = actionTiles
        , player1 = restorePlayerNextRound actions game.player1
        , player2 = restorePlayerNextRound actions game.player2
    }


nextPlayer : Game -> Game
nextPlayer ({ player1, player2 } as game) =
    { game
        | stack = [ NewActionPhase ]
        , player1 = { player1 | active = not player1.active }
        , player2 = { player2 | active = not player2.active }
    }


view : Game -> Html GameMsg
view game =
    div [ class "container" ]
        [ viewStatusBar game
        , viewActionTiles game
        , viewMain game
        ]


viewStatusBar : Game -> Html GameMsg
viewStatusBar game =
    div [ class "statusbar" ]
        [ Html.button [ onClick Pass ] [ text "Pass" ]
        , div []
            [ text
                ("Round: "
                    ++ String.fromInt game.round
                    ++ " || Player "
                    ++ String.fromInt
                        (if game.player1.active then
                            1

                         else
                            2
                        )
                    ++ " || Actions: "
                    ++ (game |> getCurrentPlayer |> .actionTiles |> List.length |> String.fromInt)
                    ++ "/"
                    ++ String.fromInt game.actions
                    ++ " || Phase: "
                    ++ (Stack.top game.stack |> subphaseToString)
                    ++ " || Available Walls: "
                    ++ (game.availableWalls |> String.fromInt)
                )
            ]
        ]


getCurrentPlayer : Game -> PlayerBoard
getCurrentPlayer game =
    if game.player1.active then
        game.player1

    else
        game.player2


opponentPlayer : Game -> PlayerBoard
opponentPlayer game =
    if game.player1.active then
        game.player2

    else
        game.player1


viewActionTiles : Game -> Html GameMsg
viewActionTiles game =
    div [ class "actiontiles" ]
        (List.map (viewActionTile game) game.actionTiles)


viewActionTile : Game -> Tile -> Html GameMsg
viewActionTile game tile =
    if Stack.top game.stack == Just NewActionPhase && tile.status == Available then
        viewTile [ class "actiontile pick", onClick (PickRoundTile tile) ] (getCurrentPlayer game).resources tile

    else
        viewTile [ class "actiontile" ] game.player1.resources tile


viewMain : Game -> Html GameMsg
viewMain game =
    div [ class "mainboard" ]
        [ viewBoard game.player1 (Stack.top game.stack)
        , viewAvailableRooms (getCurrentPlayer game) (Stack.top game.stack) game.availableRooms
        , viewChooseAdditionalCavern game
        , viewBoard game.player2 (Stack.top game.stack)
        ]


viewAvailableRooms : PlayerBoard -> Maybe Subphase -> List Tile -> Html GameMsg
viewAvailableRooms player subphase rooms =
    div [ class "availablerooms" ] (List.map (viewAvailableRoom player subphase) rooms)


viewChooseAdditionalCavern : Game -> Html GameMsg
viewChooseAdditionalCavern game =
    if Stack.top game.stack == Just SelectAdditionalCave then
        div [ class "additionalcaverns" ]
            (List.map (viewAdditionalCavern (getCurrentPlayer game)) [ tileAdditionalCavern3Walls, tileAdditionalCavern4Walls ])

    else
        Html.text ""


viewAdditionalCavern player room =
    viewTile [ class "additionalcavern pick", onClick (SelectRoomTile room) ] player.resources room


viewAvailableRoom : PlayerBoard -> Maybe Subphase -> Tile -> Html GameMsg
viewAvailableRoom player subphase room =
    if subphase == Just Furnish && isRoomSelectable player room then
        viewTile [ class "availableroom pick", onClick (SelectRoomTile room) ] player.resources room

    else
        viewTile [ class "availableroom" ] player.resources room
