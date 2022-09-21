module Main exposing (main)

import Browser
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, style)
import PlayerBoard exposing (PlayerBoard, viewBoard)
import Random
import Random.List
import Resources exposing (Resources)
import Tiles exposing (Action, ActionTile, Actions, RoomTile, tileAltareSacrificale, tileBancarella, tileCameraSegreta, tileCavaInEspansione, tileDeposito, tileFiliera, tileFoodCorner, tileForno, tileGoldMine, tileLuxuryRoom, tileMacina, tileOfficina, tilePlaceholder, tileSalotto, tileShelf, tileSpinningWheel, tileStanzaDiSnodo, tileTesoreria, tileTunnel, tileWarehouse, viewTile)
import Walls


type alias ActionBoard state =
    { actionTiles : List (ActionTile state)
    }


type alias Game =
    { activePlayer : PlayerBoard
    , waitingPlayer : PlayerBoard
    , turn : Int
    , actionBoard : ActionBoard PlayerBoard
    , availableRooms : List (RoomTile Resources)
    }


type Msg
    = DoAction (Action Resources)
    | InitPlayerBoard (List (RoomTile Resources))


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


init : () -> ( Game, Cmd Msg )
init _ =
    ( Game newBoard newBoard 1 newActionBoard newAvailableRooms
    , initRandom
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
        , tilePlaceholder
        , tilePlaceholder
        , tilePlaceholder
        , tilePlaceholder
        , tilePlaceholder
        ]
    )


initRandom : List (RoomTile Resources) -> Cmd Msg
initRandom rooms =
    Random.generate InitPlayerBoard (Random.List.shuffle rooms)


newBoard : PlayerBoard
newBoard =
    PlayerBoard (Resources 1 1 1 1 1 1) [] (List.repeat 14 Walls.None)


newActionBoard : ActionBoard PlayerBoard
newActionBoard =
    ActionBoard []


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
        [ viewStatusBar, viewActionTiles, viewMain game ]


viewStatusBar : Html Msg
viewStatusBar =
    div [ class "statusbar" ] [ text "Status Bar: Player 1, First Move" ]


viewActionTiles : Html Msg
viewActionTiles =
    div [ class "actiontiles" ] [ text "Available Actions" ]


viewMain : Game -> Html Msg
viewMain game =
    div [ class "mainboard" ]
        [ Html.map remap (viewBoard game.activePlayer)
        , viewAvailableRooms game.activePlayer.resources game.availableRooms
        , Html.map remap (viewBoard game.waitingPlayer)
        ]


viewAvailableRooms : Resources -> List (RoomTile Resources) -> Html Msg
viewAvailableRooms resources rooms =
    div [ class "availablerooms" ] (List.map (viewTile resources) rooms)


viewTile room resources =
    div [class "availableroom"] [Html.map remap (Tiles.viewTile room resources)]


remap : Tiles.Msg -> Msg
remap html =
    case html of
        Tiles.DoAction action ->
            DoAction action
