module Main exposing (main)

import Browser
import Debug exposing (toString)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import PlayerBoard exposing (Choices(..), PlayerBoard, viewBoard)
import Random
import Random.List
import Resources exposing (Resources)
import Tiles exposing (Action, Events(..), RoomTile, tileAltareSacrificale, tileAnalisiTerritoriale, tileArredare, tileBancarella, tileCameraSegreta, tileCavaInEspansione, tileColtivare, tileCostruireUnMuro, tileDemolireUnMuro, tileDeposito, tileDepositoDiLegna, tileEquipaggiamenti, tileEspansione, tileFiliera, tileFoodCorner, tileForno, tileGoldMine, tileLavorareIlLino, tileLavoriDomestici, tileLuxuryRoom, tileMacina, tileMinare, tileOfficina, tilePerforare, tilePlaceholder, tileRinnovare, tileSalotto, tileScavare, tileSetStatus, tileShelf, tileSotterraneo, tileSottobosco, tileSpedizione, tileSpinningWheel, tileStanzaDiSnodo, tileTesoreria, tileTunnel, tileWarehouse, viewTile)
import Walls


type alias Game =
    { player1 : PlayerBoard Msg
    , player2 : PlayerBoard Msg
    , turn : Int
    , phase : RoundPhase
    , subphase : Maybe Choices
    , actionTiles : List (RoomTile Resources Msg)
    , availableRooms : List (RoomTile Resources Msg)
    }


type Msg
    = InitPlayerBoard (List (RoomTile Resources Msg))
    | InitRoundTiles (List (RoomTile Resources Msg))
    | NextTurn
    | DoAction (RoomTile Resources Msg) (Action Resources Msg)
    | PickActionTile (RoomTile Resources Msg)
    | Escavate (RoomTile Resources Msg) (Action Resources Msg)
    | EscavateRoomTile (RoomTile Resources Msg)
    | DoNothing


type RoundPhase
    = NewActionPhase
    | ActionPhase


main =
    Browser.element { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }


init : () -> ( Game, Cmd Msg )
init _ =
    ( Game newBoard newBoard 1 NewActionPhase Nothing [] newAvailableRooms
    , setupRandomTiles
        [ tileWarehouse (OnActionClick DoAction)
        , tileAltareSacrificale (OnActionClick DoAction)
        , tileBancarella (OnActionClick DoAction)
        , tileCameraSegreta (OnActionClick DoAction)
        , tileCavaInEspansione (OnActionClick DoAction)
        , tileDeposito (OnActionClick DoAction)
        , tileFiliera (OnActionClick DoAction)
        , tileForno (OnActionClick DoAction)
        , tileGoldMine (OnActionClick DoAction)
        , tileOfficina (OnActionClick DoAction)
        , tileLuxuryRoom (OnActionClick DoAction)
        , tileStanzaDiSnodo (OnActionClick DoAction)
        , tileTesoreria (OnActionClick DoAction)
        , tileAnalisiTerritoriale (OnActionClick DoAction)
        , tileSotterraneo (OnActionClick DoAction)
        , tileEquipaggiamenti (OnActionClick DoAction)
        , tileLavorareIlLino (OnActionClick DoAction)
        , tileDepositoDiLegna (OnActionClick DoAction)
        ]
        [ tileLavoriDomestici (OnActionClick DoAction), tileColtivare (OnActionClick DoAction), tileSottobosco (OnActionClick DoAction), tileScavare (OnActionClick DoAction) (OnActionClick Escavate) ]
        [ tileArredare (OnActionClick DoAction), tileCostruireUnMuro (OnActionClick DoAction), tileMinare (OnActionClick DoAction) ]
        [ tileDemolireUnMuro (OnActionClick DoAction), tileEspansione (OnActionClick DoAction), tileSpedizione (OnActionClick DoAction), tilePerforare (OnActionClick DoAction) ]
        [ tileRinnovare (OnActionClick DoAction) ]
    )


setupRandomTiles : List (RoomTile Resources Msg) -> List (RoomTile Resources Msg) -> List (RoomTile Resources Msg) -> List (RoomTile Resources Msg) -> List (RoomTile Resources Msg) -> Cmd Msg
setupRandomTiles rooms round1Tiles round2Tiles round3Tiles round4Tiles =
    Cmd.batch
        [ Random.generate InitPlayerBoard (Random.List.shuffle rooms)
        , Random.generate InitRoundTiles (Random.List.shuffle round1Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round2Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round3Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round4Tiles)
        ]


newBoard : PlayerBoard Msg
newBoard =
    PlayerBoard (Resources 1 1 1 1 1 1) [] (List.repeat 14 Walls.None) []


newAvailableRooms : List (RoomTile Resources Msg)
newAvailableRooms =
    [ tileShelf (OnActionClick DoAction)
    , tileSpinningWheel (OnActionClick DoAction)
    , tileMacina (OnActionClick DoAction)
    , tileSalotto (OnActionClick DoAction)
    , tileTunnel (OnActionClick DoAction)
    , tileFoodCorner (OnActionClick DoAction)
    ]


update : Msg -> Game -> ( Game, Cmd Msg )
update msg ({ player1, player2 } as game) =
    case msg of
        InitRoundTiles tiles ->
            ( { game | actionTiles = tiles ++ game.actionTiles }, Cmd.none )

        InitPlayerBoard rooms ->
            ( { game
                | player1 = { player1 | rooms = List.take 9 rooms }
                , player2 = { player2 | rooms = List.drop 9 rooms |> List.take 9 }
              }
            , Cmd.none
            )

        DoAction tile action ->
            ( { game
                | player1 =
                    { player1
                        | resources = action.do game.player1.resources
                        , rooms = updateTile tile game.player1.rooms
                        , actionTiles = updateTile tile game.player1.actionTiles
                    }
              }
            , Cmd.none
            )

        Escavate tile action ->
            ( { game
                | subphase = Just ChooseWhichRoomToEscavate
                , player1 =
                    { player1
                        | resources = action.do game.player1.resources
                        , rooms = updateTile tile game.player1.rooms
                        , actionTiles = updateTile tile game.player1.actionTiles
                    }
              }
            , Cmd.none
            )

        EscavateRoomTile tile ->
            let
                availableRoom =
                    { tile | status = Tiles.Available }
            in
            ( { game
                | subphase = Nothing
                , availableRooms = availableRoom :: game.availableRooms
                , player1 =
                    { player1 | rooms = tileSetStatus tile Tiles.Empty game.player1.rooms }
              }
            , Cmd.none
            )

        NextTurn ->
            ( { game
                | phase = NewActionPhase
                , turn = game.turn + 1
              }
            , Cmd.none
            )

        PickActionTile tile ->
            ( { game
                | player1 = { player1 | actionTiles = { tile | status = Tiles.Active } :: player1.actionTiles }
                , phase = ActionPhase
                , actionTiles = tileSetStatus tile Tiles.Empty game.actionTiles
              }
            , Cmd.none
            )

        DoNothing ->
            ( game, Cmd.none )


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
        [ p [] [ text ("Status Bar: Player " ++ toString (modBy 2 game.turn + 1)) ]
        , p [] [ text ("Turn: " ++ toString game.turn) ]
        , p [] [ text ("Phase: " ++ toString game.phase) ]
        , p [] [ text ("Subphase: " ++ toString game.subphase) ]
        , Html.button [ onClick NextTurn ] [ text "Fine Turno" ]
        ]


viewActionTiles : Game -> Html Msg
viewActionTiles game =
    div [ class "actiontiles" ]
        (game.actionTiles
            |> List.map (viewActionTile game)
        )


viewActionTile : Game -> RoomTile Resources Msg -> Html Msg
viewActionTile game tile =
    if game.phase == NewActionPhase then
        viewTile [ class "actiontile pick", onClick (PickActionTile tile) ] game.player1.resources tile

    else
        viewTile [ class "actiontile" ] game.player1.resources tile


viewMain : Game -> Html Msg
viewMain game =
    div [ class "mainboard" ]
        [ viewBoard game.player1 game.subphase EscavateRoomTile
        , viewAvailableRooms game.player1.resources game.availableRooms
        , viewBoard game.player2 game.subphase EscavateRoomTile
        ]


viewAvailableRooms : Resources -> List (RoomTile Resources Msg) -> Html Msg
viewAvailableRooms resources rooms =
    div [ class "availablerooms" ] (List.map (viewTile [ class "availableroom" ] resources) rooms)
