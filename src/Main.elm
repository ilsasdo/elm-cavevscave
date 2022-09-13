module Main exposing (main)

import Browser
import CaveBoard exposing (CaveBoard, viewBoard)
import Debug exposing (toString)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Resources exposing (Resources)
import Tiles as Tiles exposing (Action, ActionTile, Actions, RoomTile, tileAltareSacrificale, tileBancarella, tileCameraSegreta, tileCavaInEspansione, tileCaveEntrance, tileDeposito, tileFiliera, tileFoodCorner, tileForno, tileGoldMine, tileLuxuryRoom, tileMacina, tileOfficina, tileSalotto, tileShelf, tileSpinningWheel, tileStanzaDiSnodo, tileTesoreria, tileTunnel, tileWarehouse)


type alias ActionBoard state =
    { actionTiles : List (ActionTile state)
    }


type alias Game =
    { currentPlayer : CaveBoard
    , waitingPlayer : CaveBoard
    , turn : Int
    , actionBoard : (ActionBoard CaveBoard)
    , availableRooms : List (RoomTile Resources)
    }


type Msg
    = DoAction (Action Resources)


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


init : () -> ( Game, Cmd Msg )
init _ =
    ( Game (newBoard) (newBoard) 1 (newActionBoard) (newAvailableRooms), Cmd.none )


newBoard : CaveBoard
newBoard =
    CaveBoard (Resources 1 1 1 1 1 1) []


newActionBoard : ActionBoard CaveBoard
newActionBoard =
    ActionBoard []


newAvailableRooms : List (RoomTile Resources)
newAvailableRooms =
    [tileShelf, tileSpinningWheel, tileMacina, tileSalotto, tileTunnel, tileFoodCorner]


subscriptions : Game -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Game -> ( Game, Cmd Msg )
update msg ({currentPlayer} as model) =
    case msg of
        DoAction action ->
            if action.isDoable model.currentPlayer.resources then
                ( { model | currentPlayer = { currentPlayer | resources = action.do model.currentPlayer.resources} }, Cmd.none )

            else
                ( model, Cmd.none )

view : Game -> Html Msg
view game =
    div [class "container"]
        [viewStatusBar, viewActionTiles, viewMain game]


viewStatusBar: Html Msg
viewStatusBar =
    div [class "statusbar"] [text "Status Bar: Player 1, First Move"]

viewActionTiles: Html Msg
viewActionTiles =
    div [class "actiontiles"] [text "Available Actions"]

viewMain: Game -> Html Msg
viewMain game =
    div [class "mainboard"] [
        viewBoard game.currentPlayer,
        viewAvailableRooms game.currentPlayer.resources game.availableRooms,
        viewBoard game.waitingPlayer
    ]


viewAvailableRooms: Resources -> List (RoomTile Resources) -> Html Msg
viewAvailableRooms resources rooms =
    div [class "availablerooms"] (List.map (viewTile resources) rooms)


viewTile : Resources -> (RoomTile Resources) -> Html Msg
viewTile resources tile =
    div [ style "background-image" ("url(" ++ tile.src ++ ")")
        , class "tile"] (viewActions resources tile.actions)


viewActions : Resources -> Actions Resources -> List (Html Msg)
viewActions resources (Tiles.Actions actions) =
    List.map (viewAction resources) actions


viewAction : Resources -> Action Resources -> Html Msg
viewAction resources action =
    let
       doable =
            action.isDoable resources
    in
    div
        [ class
            ("action "++(action.classes)++" "
                ++ (if doable then
                        "doable"

                    else
                        "notdoable"
                   )
            )
        , onClick (DoAction action)
        ]
        []
