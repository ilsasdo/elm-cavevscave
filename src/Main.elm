module Main exposing (main)

import Browser
import Debug exposing (toString)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import PlayerBoard exposing (PlayerBoard, Subphase(..), viewBoard)
import Random
import Random.List
import Resources exposing (Resources)
import Tiles exposing (Action, Event(..), RoomTile, TileStatus(..), tileAltareSacrificale, tileAnalisiTerritoriale, tileArredare, tileBancarella, tileCameraSegreta, tileCavaInEspansione, tileColtivare, tileCostruireUnMuro, tileDemolireUnMuro, tileDeposito, tileDepositoDiLegna, tileEquipaggiamenti, tileEspansione, tileFiliera, tileFoodCorner, tileForno, tileGoldMine, tileLavorareIlLino, tileLavoriDomestici, tileLuxuryRoom, tileMacina, tileMinare, tileOfficina, tilePerforare, tileRinnovare, tileSalotto, tileScavare, tileSetStatus, tileShelf, tileSotterraneo, tileSottobosco, tileSpedizione, tileSpinningWheel, tileStanzaDiSnodo, tileTesoreria, tileTunnel, tileWarehouse, viewTile)
import Walls


type alias Game =
    { player1 : PlayerBoard Msg
    , player2 : PlayerBoard Msg
    , round : Int -- starts with 1 ends with 8
    , actions : Int -- 2 actions for rounds 1,2,3. 3 actions for rounds 4,5,6,7. 4 actions for round 8
    , currentPlayer : Int -- 1 or 2
    , phase : RoundPhase
    , subphase : Maybe Subphase
    , actionTiles : List (RoomTile Resources Msg)
    , availableRooms : List (RoomTile Resources Msg)
    }


type Msg
    = InitPlayerBoard (List (RoomTile Resources Msg))
    | InitRoundTiles (List (RoomTile Resources Msg))
    | Pass
    | DoAction (RoomTile Resources Msg) (Action Resources Msg)
    | PickActionTile (RoomTile Resources Msg)
    | ActivateTile (Maybe Subphase) (RoomTile Resources Msg) (Action Resources Msg)
    | SelectRoomTile (RoomTile Resources Msg)
    | DoNothing


type RoundPhase
    = NewActionPhase
    | ActionPhase


main =
    Browser.element { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }


init : () -> ( Game, Cmd Msg )
init _ =
    ( Game newBoard newBoard 1 2 1 NewActionPhase Nothing [] newAvailableRooms
    , setupRandomTiles
        [ tileWarehouse (OnClick DoAction)
        , tileAltareSacrificale (OnClick DoAction)
        , tileBancarella (OnClick DoAction)
        , tileCameraSegreta (OnClick DoAction)
        , tileCavaInEspansione (OnClick DoAction)
        , tileDeposito (OnClick DoAction)
        , tileFiliera (OnClick DoAction)
        , tileForno (OnClick DoAction)
        , tileGoldMine (OnClick DoAction)
        , tileOfficina (OnClick DoAction)
        , tileLuxuryRoom (OnClick DoAction)
        , tileStanzaDiSnodo (OnClick DoAction)
        , tileTesoreria (OnClick DoAction)
        , tileAnalisiTerritoriale (OnClick DoAction)
        , tileSotterraneo (OnClick DoAction)
        , tileEquipaggiamenti (OnClick DoAction)
        , tileLavorareIlLino (OnClick DoAction)
        , tileDepositoDiLegna (OnClick DoAction)
        ]
        [ tileLavoriDomestici (OnClick DoAction)
        , tileColtivare (OnClick DoAction) (OnClick (ActivateTile (Just Activate1)))
        , tileSottobosco (OnClick DoAction)
        , tileScavare (OnClick DoAction) (OnClick (ActivateTile (Just ChooseRoomToEscavate))) (OnClick (ActivateTile (Just ChooseSecondRoomToEscavate))) ]
        [ tileArredare (OnClick DoAction), tileCostruireUnMuro (OnClick DoAction), tileMinare (OnClick DoAction) ]
        [ tileDemolireUnMuro (OnClick DoAction), tileEspansione (OnClick DoAction), tileSpedizione (OnClick DoAction), tilePerforare (OnClick DoAction) ]
        [ tileRinnovare (OnClick DoAction) ]
    )


setupRandomTiles : List (RoomTile Resources Msg) -> List (RoomTile Resources Msg) -> List (RoomTile Resources Msg) -> List (RoomTile Resources Msg) -> List (RoomTile Resources Msg) -> Cmd Msg
setupRandomTiles rooms round1Tiles round2Tiles round3Tiles round4Tiles =
    Cmd.batch
        [ Random.generate InitPlayerBoard (Random.List.shuffle rooms)
        , Random.generate InitRoundTiles (Random.List.shuffle round4Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round3Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round2Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round1Tiles)
        ]


newBoard : PlayerBoard Msg
newBoard =
    PlayerBoard (Resources 1 1 1 1 1 1) [] (List.repeat 14 Walls.None) []


newAvailableRooms : List (RoomTile Resources Msg)
newAvailableRooms =
    [ tileShelf (OnClick DoAction)
    , tileSpinningWheel (OnClick DoAction)
    , tileMacina (OnClick DoAction)
    , tileSalotto (OnClick DoAction)
    , tileTunnel (OnClick DoAction)
    , tileFoodCorner (OnClick DoAction)
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
                | player1 = { player1 | rooms = List.take 9 rooms }
                , player2 = { player2 | rooms = List.drop 9 rooms |> List.take 9 }
              }
            , Cmd.none
            )

        DoAction tile action ->
            ( game
                |> updateCurrentPlayer
                    { activePlayer
                        | resources = action.do activePlayer.resources
                        , rooms = updateTile tile activePlayer.rooms
                        , actionTiles = updateTile tile activePlayer.actionTiles
                    }
            , Cmd.none
            )

        ActivateTile subphase tile action ->
            ( { game | subphase = subphase }
                |> updateCurrentPlayer
                    { activePlayer
                        | resources = action.do activePlayer.resources
                        , rooms = updateTile tile activePlayer.rooms
                        , actionTiles = updateTile tile activePlayer.actionTiles
                    }
            , Cmd.none
            )

        SelectRoomTile tile ->
            case game.subphase of
                Just ChooseRoomToEscavate ->
                    ( escavateRoom game activePlayer tile Nothing, Cmd.none )

                Just ChooseSecondRoomToEscavate ->
                    ( escavateRoom game activePlayer tile (Just ChooseRoomToEscavate), Cmd.none )

                Just Activate1 ->
                    (activateRoom game activePlayer tile Nothing, Cmd.none)

                Just Activate2 ->
                    (activateRoom game activePlayer tile (Just Activate1), Cmd.none)

                Just Activate3 ->
                    (activateRoom game activePlayer tile (Just Activate2), Cmd.none)

                Nothing ->
                    ( game, Cmd.none )

        Pass ->
            ( pass game, Cmd.none )

        PickActionTile tile ->
            ( { game
                | phase = ActionPhase
                , actionTiles = tileSetStatus tile Tiles.Empty game.actionTiles
              }
                |> updateCurrentPlayer { activePlayer | actionTiles = { tile | status = Tiles.Active } :: activePlayer.actionTiles }
            , Cmd.none
            )

        DoNothing ->
            ( game, Cmd.none )


activateRoom game activePlayer tile subphase =
    { game
        | subphase = subphase
    }
    |> updateCurrentPlayer { activePlayer | rooms = tileSetStatus tile Tiles.Active activePlayer.rooms }

escavateRoom game activePlayer tile subphase =
    let
        availableRoom =
            { tile | status = Tiles.Available }
    in
    { game
        | subphase = subphase
        , availableRooms = availableRoom :: game.availableRooms
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
        , subphase = Nothing
        , round = round
        , actions = actions
        , actionTiles = actionTiles
        , player1 = restorePlayer game.player1
        , player2 = restorePlayer game.player2
    }


restorePlayer : PlayerBoard Msg -> PlayerBoard Msg
restorePlayer player =
    { player | actionTiles = [], rooms = List.map restoreRoom player.rooms }


restoreRoom : RoomTile Resources Msg -> RoomTile Resources Msg
restoreRoom room =
    if room.status == Active then
        { room | status = Available }

    else
        room


nextPlayer : Game -> Game
nextPlayer game =
    { game
        | phase = NewActionPhase
        , subphase = Nothing
        , currentPlayer =
            if game.currentPlayer == 1 then
                2

            else
                1
    }


updateTile : RoomTile Resources Msg -> List (RoomTile Resources Msg) -> List (RoomTile Resources Msg)
updateTile tile tiles =
    List.map
        (\r ->
            if r.title == tile.title then
                tile

            else
                r
        )
        tiles


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
        , p [] [ text ("Subphase: " ++ toString game.subphase) ]
        , Html.button [ onClick Pass ] [ text "Pass" ]
        ]


currentPlayer : Game -> PlayerBoard Msg
currentPlayer game =
    if game.currentPlayer == 1 then
        game.player1

    else
        game.player2


updateCurrentPlayer : PlayerBoard Msg -> Game -> Game
updateCurrentPlayer player game =
    if game.currentPlayer == 1 then
        { game | player1 = player }

    else
        { game | player2 = player }


viewActionTiles : Game -> Html Msg
viewActionTiles game =
    div [ class "actiontiles" ]
        (List.map (viewActionTile game) game.actionTiles)


viewActionTile : Game -> RoomTile Resources Msg -> Html Msg
viewActionTile game tile =
    if game.phase == NewActionPhase && tile.status == Available then
        viewTile [ class "actiontile pick", onClick (PickActionTile tile) ] (currentPlayer game).resources tile

    else
        viewTile [ class "actiontile" ] game.player1.resources tile


viewMain : Game -> Html Msg
viewMain game =
    div [ class "mainboard" ]
        [ viewBoard game.player1 game.subphase SelectRoomTile
        , viewAvailableRooms (currentPlayer game).resources game.availableRooms
        , viewBoard game.player2 game.subphase SelectRoomTile
        ]


viewAvailableRooms : Resources -> List (RoomTile Resources Msg) -> Html Msg
viewAvailableRooms resources rooms =
    div [ class "availablerooms" ] (List.map (viewTile [ class "availableroom" ] resources) rooms)
