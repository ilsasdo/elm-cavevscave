module Main exposing (main)

import Browser
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, style)
import PlayerBoard exposing (PlayerBoard, viewBoard)
import Random
import Random.List
import Resources exposing (Resources)
import Tiles exposing (Action, ActionTile, Actions, RoomTile, tileAltareSacrificale, tileAnalisiTerritoriale, tileArredare, tileBancarella, tileCameraSegreta, tileCavaInEspansione, tileColtivare, tileCostruireUnMuro, tileDemolireUnMuro, tileDeposito, tileDepositoDiLegna, tileEquipaggiamenti, tileEspansione, tileFiliera, tileFoodCorner, tileForno, tileGoldMine, tileLavorareIlLino, tileLavoriDomestici, tileLuxuryRoom, tileMacina, tileMinare, tileOfficina, tilePerforare, tilePlaceholder, tileRinnovare, tileSalotto, tileScavare, tileShelf, tileSotterraneo, tileSottobosco, tileSpedizione, tileSpinningWheel, tileStanzaDiSnodo, tileTesoreria, tileTunnel, tileWarehouse, viewTile)
import Walls


type alias Game =
    { activePlayer : PlayerBoard
    , waitingPlayer : PlayerBoard
    , turn : Int
    , roundTiles : List (RoomTile Resources)
    , availableRooms : List (RoomTile Resources)
    }


type Msg
    = DoAction (Action Resources)
    | InitPlayerBoard (List (RoomTile Resources))
    | InitRoundTiles (List (RoomTile Resources))


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


init : () -> ( Game, Cmd Msg )
init _ =
    ( Game newBoard newBoard 1 [] newAvailableRooms
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
        [tileLavoriDomestici, tileColtivare, tileSottobosco, tileScavare]
        [tileArredare, tileCostruireUnMuro, tileMinare]
        [tileDemolireUnMuro, tileEspansione, tileSpedizione, tilePerforare]
        [tileRinnovare]
    )


setupRandomTiles : List (RoomTile Resources) -> List (RoomTile Resources) -> List (RoomTile Resources) -> List (RoomTile Resources) -> List (RoomTile Resources) -> Cmd Msg
setupRandomTiles rooms round1Tiles round2Tiles round3Tiles round4Tiles =
    Cmd.batch [
          Random.generate InitPlayerBoard (Random.List.shuffle rooms)
        , Random.generate InitRoundTiles (Random.List.shuffle round1Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round2Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round3Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round4Tiles)
        ]


newBoard : PlayerBoard
newBoard =
    PlayerBoard (Resources 1 1 1 1 1 1) [] (List.repeat 14 Walls.None)


newAvailableRooms : List (RoomTile Resources)
newAvailableRooms =
    [ tileShelf
    , tileSpinningWheel
    , tileMacina
    , tileSalotto
    , tileTunnel
    , tileFoodCorner
    ]


subscriptions : Game -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Game -> ( Game, Cmd Msg )
update msg ({ activePlayer, waitingPlayer } as model) =
    case msg of
        InitRoundTiles tiles ->
            ( {model | roundTiles = tiles ++ model.roundTiles }, Cmd.none)

        InitPlayerBoard rooms ->
            ( { model
                | activePlayer = { activePlayer | rooms = List.take 9 rooms }
                , waitingPlayer = { waitingPlayer | rooms = List.drop 9 rooms |> List.take 9 }
              }
            , Cmd.none
            )

        DoAction action ->
            if action.isDoable model.activePlayer.resources then
                ( { model | activePlayer = { activePlayer | resources = action.do model.activePlayer.resources } }, Cmd.none )

            else
                ( model, Cmd.none )


view : Game -> Html Msg
view game =
    div [ class "container" ]
        [ viewStatusBar, viewActionTiles game, viewMain game ]


viewStatusBar : Html Msg
viewStatusBar =
    div [ class "statusbar" ] [ text "Status Bar: Player 1, First Move" ]


viewActionTiles : Game -> Html Msg
viewActionTiles game =
    div [ class "actiontiles" ]
        (game.roundTiles
            |> List.map (viewActionTile game))


viewActionTile : Game -> RoomTile Resources -> Html Msg
viewActionTile game tile =
    viewTile "actiontile" game.activePlayer.resources tile


viewMain : Game -> Html Msg
viewMain game =
    div [ class "mainboard" ]
        [ Html.map remap (viewBoard game.activePlayer)
        , viewAvailableRooms game.activePlayer.resources game.availableRooms
        , Html.map remap (viewBoard game.waitingPlayer)
        ]


viewAvailableRooms : Resources -> List (RoomTile Resources) -> Html Msg
viewAvailableRooms resources rooms =
    div [ class "availablerooms" ] (List.map (viewTile "availableroom" resources) rooms)


viewTile className room resources =
    div [class className] [Html.map remap (Tiles.viewTile room resources)]


remap : Tiles.Msg -> Msg
remap html =
    case html of
        Tiles.DoAction action ->
            DoAction action
