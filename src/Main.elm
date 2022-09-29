module Main exposing (main)

import Browser
import Debug exposing (toString)
import Html exposing (Html, div, input, p, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import PlayerBoard exposing (PlayerBoard, viewBoard)
import Random
import Random.List
import Resources exposing (Resources)
import Tiles exposing (Action, ActionTile, Actions, RoomTile, tileAltareSacrificale, tileAnalisiTerritoriale, tileArredare, tileBancarella, tileCameraSegreta, tileCavaInEspansione, tileColtivare, tileCostruireUnMuro, tileDemolireUnMuro, tileDeposito, tileDepositoDiLegna, tileEquipaggiamenti, tileEspansione, tileFiliera, tileFoodCorner, tileForno, tileGoldMine, tileLavorareIlLino, tileLavoriDomestici, tileLuxuryRoom, tileMacina, tileMinare, tileOfficina, tilePerforare, tilePlaceholder, tileRinnovare, tileSalotto, tileScavare, tileShelf, tileSotterraneo, tileSottobosco, tileSpedizione, tileSpinningWheel, tileStanzaDiSnodo, tileTesoreria, tileTunnel, tileWarehouse, viewTile)
import Walls


type alias Game =
    { player1 : PlayerBoard
    , player2 : PlayerBoard
    , turn : Int
    , phase : RoundPhase
    , actionTiles : List (RoomTile Resources)
    , availableRooms : List (RoomTile Resources)
    }


type Msg
    = DoAction (RoomTile Resources) (Action Resources)
    | InitPlayerBoard (List (RoomTile Resources))
    | InitRoundTiles (List (RoomTile Resources))
    | NextTurn
    | PickActionTile (RoomTile Resources)
    | Escavate
    | DoNothing


type RoundPhase
    = NewActionPhase
    | ActionPhase


main =
    Browser.element { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }


init : () -> ( Game, Cmd Msg )
init _ =
    ( Game newBoard newBoard 1 NewActionPhase [] newAvailableRooms
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
        [ tileLavoriDomestici, tileColtivare, tileSottobosco, tileScavare ]
        [ tileArredare, tileCostruireUnMuro, tileMinare ]
        [ tileDemolireUnMuro, tileEspansione, tileSpedizione, tilePerforare ]
        [ tileRinnovare ]
    )


setupRandomTiles : List (RoomTile Resources) -> List (RoomTile Resources) -> List (RoomTile Resources) -> List (RoomTile Resources) -> List (RoomTile Resources) -> Cmd Msg
setupRandomTiles rooms round1Tiles round2Tiles round3Tiles round4Tiles =
    Cmd.batch
        [ Random.generate InitPlayerBoard (Random.List.shuffle rooms)
        , Random.generate InitRoundTiles (Random.List.shuffle round1Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round2Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round3Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round4Tiles)
        ]


newBoard : PlayerBoard
newBoard =
    PlayerBoard (Resources 1 1 1 1 1 1) [] (List.repeat 14 Walls.None) []


newAvailableRooms : List (RoomTile Resources)
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
            let
                (res, cmd) = action.do game.player1.resources
            in
            if action.isDoable game.player1.resources then
                ( { game
                    | player1 =
                        { player1
                            | resources = res
                            , rooms = updateTile tile game.player1.rooms
                            , actionTiles = updateTile tile game.player1.actionTiles
                        }
                  }
                , Cmd.none
                )

            else
                ( game, Cmd.none )

        NextTurn ->
            ( { game
                | phase = NewActionPhase
                , turn = game.turn + 1
              }
            , Cmd.none
            )

        PickActionTile tile ->
            ( { game
                | player1 = { player1 | actionTiles = { tile | active = True } :: player1.actionTiles }
                , actionTiles =
                    List.map
                        (\t ->
                            if t == tile then
                                { t | available = False }

                            else
                                t
                        )
                        game.actionTiles
                , phase = ActionPhase
              }
            , Cmd.none
            )

        Escavate ->
            (game, Cmd.none)

        DoNothing ->
            (game, Cmd.none)


updateTile: RoomTile Resources -> List (RoomTile Resources) -> List (RoomTile Resources)
updateTile tile tiles =
    let
       t = Debug.log "tile" tile
    in
    List.map (\r -> if r.title == tile.title then tile else r) tiles


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
        , Html.button [ onClick NextTurn ] [ text "Fine Turno" ]
        ]


viewActionTiles : Game -> Html Msg
viewActionTiles game =
    div [ class "actiontiles" ]
        (game.actionTiles
            |> List.map (viewActionTile game)
        )


viewActionTile : Game -> RoomTile Resources -> Html Msg
viewActionTile game tile =
    if game.phase == NewActionPhase then
        viewTile [ class "actiontile pick", onClick (PickActionTile tile) ] game.player1.resources tile

    else
        viewTile [ class "actiontile" ] game.player1.resources tile


viewMain : Game -> Html Msg
viewMain game =
    div [ class "mainboard" ]
        [ Html.map remap (viewBoard game.player1)
        , viewAvailableRooms game.player1.resources game.availableRooms
        , Html.map remap (viewBoard game.player2)
        ]


viewAvailableRooms : Resources -> List (RoomTile Resources) -> Html Msg
viewAvailableRooms resources rooms =
    div [ class "availablerooms" ] (List.map (viewTile [ class "availableroom" ] resources) rooms)


viewTile attributes room resources =
    div attributes [ Html.map remap (Tiles.viewTile room resources) ]


remap : Tiles.Msg -> Msg
remap html =
    case html of
        Tiles.DoAction tile action ->
            DoAction tile action

        _ ->
            DoNothing
