module Main exposing (main)

import Browser
import Game exposing (..)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import PlayerBoard exposing (isRoomSelectable, restorePlayerNextRound, restorePlayerPass, updateTile, viewBoard)
import Resources exposing (addFood, updateOpponentsGold)
import Stack
import Tiles exposing (tileProspectingSite, tileSottobosco, viewTile)


main =
    Browser.element { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }


init : () -> ( Game, Cmd GameMsg )
init _ =
    ( Game PlayerBoard.newBoard PlayerBoard.newBoard 1 2 1 NewActionPhase [] Tiles.initCommonRooms 7, Tiles.initRandomTiles )


update : GameMsg -> Game -> ( Game, Cmd GameMsg )
update msg ({ player1, player2 } as game) =
    let
        activePlayer =
            getCurrentPlayer game
    in
    case msg of
        InitRoundTiles tiles ->
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

        AddToAvailableRooms tile ->
            ( game |> addToAvailableRooms tile, Cmd.none )

        RemoveFromAvailableRooms tile ->
            ( game |> removeFromAvailableRooms tile, Cmd.none )

        WallBuilt ->
            game
                |> getCurrentPlayer
                |> PlayerBoard.applyDungeon
                |> setCurrentPlayer2 game
                |> updateAvailableWalls -1
                |> Tuple.pair Cmd.none
                |> swap

        WallDestroyed ->
            ( game |> updateAvailableWalls 1, Cmd.none )

        Pass ->
            ( pass game, Cmd.none )

        PickRoundTile tile ->
            ( pickActionTile game activePlayer tile
            , Cmd.none
            )

        DoAction tile action ->
            let
                consumedTile =
                    Tiles.consumeAction tile action
            in
            ( setCurrentPlayer
                { activePlayer
                    | resources = action.do activePlayer.resources |> PlayerBoard.applyRettingRoom activePlayer
                    , subphase = Maybe.withDefault activePlayer.subphase (Just action.subphase)
                    , rooms = updateTile consumedTile activePlayer.rooms
                    , actionTiles = updateTile consumedTile activePlayer.actionTiles
                }
                game
            , Cmd.none
            )

        SelectWall index ->
            case activePlayer.subphase of
                Just BuildWall ->
                    activePlayer
                        |> PlayerBoard.buildWall index
                        |> setCurrentPlayer2 game
                        |> update WallBuilt

                Just DestroyWall ->
                    activePlayer
                        |> PlayerBoard.destroyWall index
                        |> setCurrentPlayer2 game
                        |> update WallDestroyed

                _ ->
                    ( game, Cmd.none )

        SelectRoomTile tile ->
            case activePlayer.subphase of
                Just Furnish ->
                    ( setCurrentPlayer { activePlayer | subphase = Just (PlaceRoom tile) } game, Cmd.none )

                Just (PlaceRoom tileToPlace) ->
                    activePlayer
                        |> PlayerBoard.placeRoom tile tileToPlace
                        |> setCurrentPlayer2 game
                        |> update (RemoveFromAvailableRooms tileToPlace)

                Just ExcavateThroughWall ->
                    activePlayer
                        |> PlayerBoard.escavateRoom tile Nothing
                        |> setCurrentPlayer2 game
                        |> update (AddToAvailableRooms tile)

                Just Excavate1 ->
                    activePlayer
                        |> PlayerBoard.escavateRoom tile Nothing
                        |> setCurrentPlayer2 game
                        |> update (AddToAvailableRooms tile)

                Just Excavate2 ->
                    activePlayer
                        |> PlayerBoard.escavateRoom tile (Just Excavate1)
                        |> setCurrentPlayer2 game
                        |> update (AddToAvailableRooms tile)

                Just (Activate1 first) ->
                    activePlayer
                        |> PlayerBoard.applyWoodStoreroom first 1
                        |> PlayerBoard.activateRoom tile Nothing
                        |> updateGame game

                Just (Activate2 first) ->
                    activePlayer
                        |> PlayerBoard.activateRoom tile (PlayerBoard.applyEquipmentRoom (Activate2 first) activePlayer)
                        |> updateGame game

                Just (Activate3 first) ->
                    activePlayer
                        |> PlayerBoard.activateRoom tile (PlayerBoard.applyEquipmentRoom (Activate3 first) activePlayer)
                        |> updateGame game

                _ ->
                    ( game, Cmd.none )

        ResourceChosen maybeSubphase updateResources ->
            ( setCurrentPlayer
                { activePlayer
                    | resources = updateResources activePlayer.resources
                    , subphase = maybeSubphase
                }
                game
            , Cmd.none
            )


swap ( a, b ) =
    ( b, a )


setCurrentPlayer2 player game =
    setCurrentPlayer game player


updateGame game player =
    ( setCurrentPlayer player game, Cmd.none )


pickActionTile game activePlayer tile =
    let
        player =
            { activePlayer | actionTiles = { tile | status = Active } :: activePlayer.actionTiles }
    in
    { game
        | phase = ActionPhase
        , actionTiles = Tiles.updateStatus tile Empty game.actionTiles
    }
        |> setCurrentPlayer player
        |> activateProspectingSite player tile


activateProspectingSite : PlayerBoard -> Tile -> Game -> Game
activateProspectingSite player tile game =
    if tile.title == tileSottobosco.title && PlayerBoard.playerHasEquipment player tileProspectingSite then
        setCurrentPlayer (PlayerBoard.activateRoom tileProspectingSite Nothing player) game

    else
        game


pass : Game -> Game
pass game =
    if List.length game.player1.actionTiles == game.actions && List.length game.player2.actionTiles == game.actions then
        nextRound game

    else
        game
            |> setCurrentPlayer (restorePlayerPass (getCurrentPlayer game))
            |> nextPlayer
            |> activatePlayer


activatePlayer : Game -> Game
activatePlayer game =
    let
        player =
            getCurrentPlayer game

        opponent =
            opponentPlayer game
    in
    setCurrentPlayer
        { player
            | freeAction = player.freeAction |> PlayerBoard.restoreTile |> Tiles.setStatus Active
            , resources = updateOpponentsGold opponent.resources.gold player.resources
        }
        game


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
        | phase = NewActionPhase
        , round = round
        , actions = actions
        , actionTiles = actionTiles
        , player1 = restorePlayerNextRound game.player1 actions
        , player2 = restorePlayerNextRound game.player2 actions
    }


nextPlayer : Game -> Game
nextPlayer game =
    { game
        | phase = NewActionPhase
        , currentPlayer =
            if game.currentPlayer == 1 then
                2

            else
                1
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
                    ++ String.fromInt game.currentPlayer
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
    if game.currentPlayer == 1 then
        game.player1

    else
        game.player2


opponentPlayer : Game -> PlayerBoard
opponentPlayer game =
    if game.currentPlayer == 1 then
        game.player2

    else
        game.player1


setCurrentPlayer : PlayerBoard -> Game -> Game
setCurrentPlayer player game =
    if game.currentPlayer == 1 then
        { game | player1 = player }

    else
        { game | player2 = player }


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
        [ viewBoard game.player1
        , viewAvailableRooms (getCurrentPlayer game) game.availableRooms
        , viewBoard game.player2
        ]


viewAvailableRooms : PlayerBoard -> List Tile -> Html GameMsg
viewAvailableRooms player rooms =
    div [ class "availablerooms" ] (List.map (viewAvailableRoom player) rooms)


viewAvailableRoom : PlayerBoard -> Tile -> Html GameMsg
viewAvailableRoom player room =
    if player.subphase == Just Furnish && isRoomSelectable player room then
        viewTile [ class "availableroom pick", onClick (SelectRoomTile room) ] player.resources room

    else
        viewTile [ class "availableroom" ] player.resources room
