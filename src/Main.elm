module Main exposing (main)

import Browser
import Debug exposing (toString)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import PlayerBoard exposing (PlayerBoard, viewBoard)
import Random
import Random.List
import Resources exposing (Resources)
import Tiles exposing (Action, Msg(..), Subphase(..), Tile, TileStatus(..), tileAltareSacrificale, tileAnalisiTerritoriale, tileArredare, tileBancarella, tileCameraSegreta, tileCavaInEspansione, tileCaveEntrance, tileColtivare, tileCostruireUnMuro, tileDemolireUnMuro, tileDeposito, tileDepositoDiLegna, tileEmpty, tileEquipaggiamenti, tileEspansione, tileFiliera, tileFoodCorner, tileForno, tileGoldMine, tileLavorareIlLino, tileLavoriDomestici, tileLuxuryRoom, tileMacina, tileMinare, tileOfficina, tilePerforare, tileRinnovare, tileSalotto, tileScavare, tileSetStatus, tileShelf, tileSotterraneo, tileSottobosco, tileSpedizione, tileSpinningWheel, tileStanzaDiSnodo, tileTesoreria, tileTunnel, tileWarehouse, viewTile)
import Walls


type alias Game =
    { player1 : PlayerBoard
    , player2 : PlayerBoard
    , round : Int -- starts with 1 ends with 8
    , actions : Int -- 2 actions for rounds 1,2,3. 3 actions for rounds 4,5,6,7. 4 actions for round 8
    , currentPlayer : Int -- 1 or 2
    , phase : RoundPhase
    , actionTiles : List Tile
    , availableRooms : List Tile
    }


type Msg
    = InitPlayerBoard (List Tile)
    | InitRoundTiles (List Tile)
    | PlayerMsg Tiles.Msg
    | PickRoundTile Tile
    | Pass


type RoundPhase
    = NewActionPhase
    | ActionPhase


main =
    Browser.element { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }


init : () -> ( Game, Cmd Msg )
init _ =
    ( Game newBoard newBoard 1 2 1 NewActionPhase [] newAvailableRooms
    , setupRandomTiles
        [ tileWarehouse
        , tileAltareSacrificale
        , tileBancarella
        , tileCameraSegreta
        , tileCavaInEspansione
        , tileDeposito
        , tileFiliera
        , tileForno
        , tileGoldMine
        , tileOfficina
        , tileLuxuryRoom
        , tileStanzaDiSnodo
        , tileTesoreria
        , tileAnalisiTerritoriale
        , tileSotterraneo
        , tileEquipaggiamenti
        , tileLavorareIlLino
        , tileDepositoDiLegna
        ]
        [ tileLavoriDomestici
        , tileColtivare
        , tileSottobosco
        , tileScavare
        ]
        [ tileArredare
        , tileCostruireUnMuro
        , tileMinare
        ]
        [ tileDemolireUnMuro
        , tileEspansione
        , tileSpedizione
        , tilePerforare
        ]
        [ tileRinnovare ]
    )


setupRandomTiles : List Tile -> List Tile -> List Tile -> List Tile -> List Tile -> Cmd Msg
setupRandomTiles rooms round1Tiles round2Tiles round3Tiles round4Tiles =
    Cmd.batch
        [ Random.generate InitPlayerBoard (Random.List.shuffle rooms)
        , Random.generate InitRoundTiles (Random.List.shuffle round4Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round3Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round2Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round1Tiles)
        ]


newBoard : PlayerBoard
newBoard =
    PlayerBoard (Resources 1 1 1 1 1 1 1) [] (List.repeat 14 Walls.None) [] Nothing


newAvailableRooms : List Tile
newAvailableRooms =
    [ tileShelf
    , tileSpinningWheel
    , tileMacina
    , tileSalotto
    , tileTunnel
    , tileFoodCorner
    ]


update : Msg -> Game -> ( Game, Cmd Msg )
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
                | player1 = { player1 | rooms = List.take 9 rooms |> initPlayerBoardRooms }
                , player2 = { player2 | rooms = List.drop 9 rooms |> List.take 9 |> initPlayerBoardRooms }
              }
            , Cmd.none
            )

        PlayerMsg tileMsg ->
            ( updateCurrentPlayer (PlayerBoard.update tileMsg activePlayer) game
            , Cmd.none
            )

        Pass ->
            ( pass game, Cmd.none )

        PickRoundTile tile ->
            ( { game
                | phase = ActionPhase
                , actionTiles = tileSetStatus tile Tiles.Empty game.actionTiles
              }
                |> updateCurrentPlayer { activePlayer | actionTiles = { tile | status = Tiles.Active } :: activePlayer.actionTiles }
            , Cmd.none
            )


initPlayerBoardRooms rooms =
    List.take 4 rooms ++ [ tileEmpty ] ++ (rooms |> List.drop 4 |> List.take 1) ++ [ tileCaveEntrance ] ++ List.drop 5 rooms


furnishCave game activePlayer tile tileToPlace =
    { game
        | availableRooms = List.filter (\t -> t.title /= tileToPlace.title) game.availableRooms
    }
        |> updateCurrentPlayer
            { activePlayer
                | rooms =
                    List.map
                        (\r ->
                            if r.title == tile.title then
                                tileToPlace

                            else
                                r
                        )
                        activePlayer.rooms
            }


activateRoom game activePlayer tile subphase =
    updateCurrentPlayer { activePlayer | rooms = tileSetStatus tile Tiles.Active activePlayer.rooms } game


escavateRoom game activePlayer tile subphase =
    let
        availableRoom =
            { tile | status = Tiles.Available }
    in
    { game
        | availableRooms = availableRoom :: game.availableRooms
    }
        |> updateCurrentPlayer { activePlayer | rooms = tileSetStatus tile Tiles.Empty activePlayer.rooms }


pass : Game -> Game
pass game =
    if List.length game.player1.actionTiles == game.actions && List.length game.player2.actionTiles == game.actions then
        nextRound game

    else
        nextPlayer game


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
        , player1 = restorePlayer game.player1 actions
        , player2 = restorePlayer game.player2 actions
    }


restorePlayer : PlayerBoard -> Int -> PlayerBoard
restorePlayer ({ resources } as player) round =
    { player
        | actionTiles = []
        , rooms = List.map restoreRoom player.rooms
        , resources = { resources | actions = round }
    }


restoreRoom : Tile -> Tile
restoreRoom room =
    if room.status == Active then
        { room | status = Available }

    else
        room


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


view : Game -> Html Msg
view game =
    div [ class "container" ]
        [ viewStatusBar game, viewActionTiles game, viewMain game ]


viewStatusBar : Game -> Html Msg
viewStatusBar game =
    div [ class "statusbar" ]
        [ p [] [ text ("Status Bar: Player " ++ toString game.currentPlayer ++ " Actions: " ++ (game |> currentPlayer |> .actionTiles |> List.length |> toString) ++ "/" ++ toString game.actions) ]
        , p [] [ text ("Round: " ++ toString game.round) ]
        , p [] [ text ("Phase: " ++ toString game.phase) ]
        , p [] [ text ("Subphase: " ++ (game |> currentPlayer |> .subphase |> toString)) ]
        , Html.button [ onClick Pass ] [ text "Pass" ]
        ]


currentPlayer : Game -> PlayerBoard
currentPlayer game =
    if game.currentPlayer == 1 then
        game.player1

    else
        game.player2


updateCurrentPlayer : PlayerBoard -> Game -> Game
updateCurrentPlayer player game =
    if game.currentPlayer == 1 then
        { game | player1 = player }

    else
        { game | player2 = player }


viewActionTiles : Game -> Html Msg
viewActionTiles game =
    div [ class "actiontiles" ]
        (List.map (viewActionTile game) game.actionTiles)


viewActionTile : Game -> Tile -> Html Msg
viewActionTile game tile =
    if game.phase == NewActionPhase && tile.status == Available then
        Html.map mapToPickRoundTile (viewTile [ class "actiontile pick", onClick (SelectRoomTile tile) ] (currentPlayer game).resources tile)

    else
        Html.map PlayerMsg (viewTile [ class "actiontile" ] game.player1.resources tile)

mapToPickRoundTile msg =
    case msg of
        Tiles.SelectRoomTile tile ->
            PickRoundTile tile
        _ ->
            PlayerMsg msg

viewMain : Game -> Html Msg
viewMain game =
    Html.map PlayerMsg
        (div [ class "mainboard" ]
            [ viewBoard game.player1 game.player1.subphase SelectRoomTile
            , viewAvailableRooms (currentPlayer game).resources (game |> currentPlayer |> .subphase) game.availableRooms
            , viewBoard game.player2 game.player2.subphase SelectRoomTile
            ]
        )


viewAvailableRooms : Resources -> Maybe Subphase -> List Tile -> Html Tiles.Msg
viewAvailableRooms resources subphase rooms =
    div [ class "availablerooms" ] (List.map (viewAvailableRoom resources subphase) rooms)


viewAvailableRoom : Resources -> Maybe Subphase -> Tile -> Html Tiles.Msg
viewAvailableRoom resources subphase room =
    if subphase == Just Furnish then
        viewTile [ class "availableroom pick", onClick (SelectRoomTile room) ] resources room

    else
        viewTile [ class "availableroom" ] resources room
