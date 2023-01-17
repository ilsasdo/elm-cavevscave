module Main exposing (main)

import Browser
import Game exposing (..)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import PlayerBoard exposing (isRoomSelectable, restorePlayerNextRound, restorePlayerPass, updateTile, viewBoard)
import Resources exposing (updateOpponentsGold)
import Tiles exposing (viewTile)


main =
    Browser.element { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }


init : () -> ( Game, Cmd GameMsg )
init _ =
    ( Game PlayerBoard.newBoard PlayerBoard.newBoard 1 2 1 NewActionPhase [] Tiles.initCommonRooms 7, Tiles.initRandomTiles )


update : GameMsg -> Game -> ( Game, Cmd GameMsg )
update msg ({ player1, player2 } as game) =
    let
        activePlayer =
            currentPlayer game
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
            ( game |> updateCurrentPlayer activePlayer |> addToAvailableRooms tile, Cmd.none )

        RemoveFromAvailableRooms tile ->
            ( game |> updateCurrentPlayer activePlayer |> removeFromAvailableRooms tile, Cmd.none )

        WallBuilt ->
            ( game |> updateCurrentPlayer activePlayer |> updateAvailableWalls -1, Cmd.none )

        WallDestroyed ->
            ( game |> updateCurrentPlayer activePlayer |> updateAvailableWalls 1, Cmd.none )

        Pass ->
            ( pass game, Cmd.none )

        PickRoundTile tile ->
            ( { game
                | phase = ActionPhase
                , actionTiles = Tiles.updateStatus tile Empty game.actionTiles
              }
                |> updateCurrentPlayer { activePlayer | actionTiles = { tile | status = Active } :: activePlayer.actionTiles }
            , Cmd.none
            )

        DoAction tile action ->
            let
                consumedTile =
                    Tiles.consumeAction tile action
            in
            ( updateCurrentPlayer
                { activePlayer
                    | resources = action.do activePlayer.resources
                    , subphase = action.subphase
                    , rooms = updateTile consumedTile activePlayer.rooms
                    , actionTiles = updateTile consumedTile activePlayer.actionTiles
                }
                game
            , Cmd.none
            )

        SelectWall index ->
            case activePlayer.subphase of
                Just BuildWall ->
                    update WallBuilt (updateCurrentPlayer (PlayerBoard.buildWall activePlayer index) game)

                Just DestroyWall ->
                    update WallDestroyed (updateCurrentPlayer (PlayerBoard.destroyWall activePlayer index) game)

                _ ->
                    ( game, Cmd.none )

        SelectRoomTile tile ->
            case activePlayer.subphase of
                Just Furnish ->
                    ( updateCurrentPlayer { activePlayer | subphase = Just (PlaceRoom tile) } game, Cmd.none )

                Just (PlaceRoom tileToPlace) ->
                    update (RemoveFromAvailableRooms tileToPlace) (updateCurrentPlayer (PlayerBoard.placeRoom activePlayer tile tileToPlace) game)

                Just EscavateThroughWall ->
                    update (AddToAvailableRooms tile) (updateCurrentPlayer (PlayerBoard.escavateRoom activePlayer tile Nothing) game)

                Just Escavate1 ->
                    update (AddToAvailableRooms tile) (updateCurrentPlayer (PlayerBoard.escavateRoom activePlayer tile Nothing) game)

                Just Escavate2 ->
                    update (AddToAvailableRooms tile) (updateCurrentPlayer (PlayerBoard.escavateRoom activePlayer tile (Just Escavate1)) game)

                Just Activate1 ->
                    ( updateCurrentPlayer (PlayerBoard.activateRoom activePlayer tile Nothing) game, Cmd.none )

                Just Activate2 ->
                    ( updateCurrentPlayer (PlayerBoard.activateRoom activePlayer tile (Just Activate1)) game, Cmd.none )

                Just Activate3 ->
                    ( updateCurrentPlayer (PlayerBoard.activateRoom activePlayer tile (Just Activate2)) game, Cmd.none )

                _ ->
                    ( game, Cmd.none )

        ResourceChosen maybeSubphase function ->
            ( game, Cmd.none )


pass : Game -> Game
pass game =
    if List.length game.player1.actionTiles == game.actions && List.length game.player2.actionTiles == game.actions then
        nextRound game

    else
        game
            |> updateCurrentPlayer (restorePlayerPass (currentPlayer game))
            |> nextPlayer
            |> activatePlayer


activatePlayer : Game -> Game
activatePlayer game =
    let
        player =
            currentPlayer game

        opponent =
            opponentPlayer game
    in
    updateCurrentPlayer
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
                    ++ (game |> currentPlayer |> .actionTiles |> List.length |> String.fromInt)
                    ++ "/"
                    ++ String.fromInt game.actions
                    ++ " || Phase: "
                    ++ roundPhaseToString game.phase
                    ++ " || Subphase: "
                    ++ (game |> currentPlayer |> .subphase |> subphaseToString)
                    ++ " || Available Walls: "
                    ++ (game.availableWalls |> String.fromInt)
                )
            ]
        ]


currentPlayer : Game -> PlayerBoard
currentPlayer game =
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


updateCurrentPlayer : PlayerBoard -> Game -> Game
updateCurrentPlayer player game =
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
    if game.phase == NewActionPhase && tile.status == Available then
        viewTile [ class "actiontile pick", onClick (PickRoundTile tile) ] (currentPlayer game).resources tile

    else
        viewTile [ class "actiontile" ] game.player1.resources tile


viewMain : Game -> Html GameMsg
viewMain game =
    div [ class "mainboard" ]
        [ viewBoard game.player1
        , viewAvailableRooms (currentPlayer game) game.availableRooms
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
