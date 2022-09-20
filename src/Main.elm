module Main exposing (main)

import Array
import Browser
import PlayerBoard exposing (PlayerBoard, viewBoard)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, style)
import Random
import Random.Set as Random
import Resources exposing (Resources)
import Set exposing (Set)
import Tiles exposing (Action, ActionTile, Actions, RoomTile, tileAltareSacrificale, tileBancarella, tileCameraSegreta, tileCavaInEspansione, tileCaveEntrance, tileDeposito, tileFiliera, tileFoodCorner, tileForno, tileGoldMine, tileLuxuryRoom, tileMacina, tileOfficina, tilePlaceholder, tileSalotto, tileShelf, tileSpinningWheel, tileStanzaDiSnodo, tileTesoreria, tileTunnel, tileWarehouse, viewTile)


type alias ActionBoard state =
    { actionTiles : List (ActionTile state)
    }


type alias Game =
    { activePlayer : PlayerBoard
    , waitingPlayer : PlayerBoard
    , turn : Int
    , actionBoard : (ActionBoard PlayerBoard)
    , availableRooms : List (RoomTile Resources)
    }


type Msg
    = DoAction (Action Resources)
     | InitPlayerBoard Int

main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


init : () -> ( Game, Cmd Msg )
init _ =
    ( Game (newBoard) (newBoard) 1 (newActionBoard) (newAvailableRooms), (initRandom 18) )

initRandom: Int -> Cmd Msg
initRandom max =
    Random.generate InitPlayerBoard (Random.int 0 max)

newBoard : PlayerBoard
newBoard =
    PlayerBoard (Resources 1 1 1 1 1 1) []


newActionBoard : ActionBoard PlayerBoard
newActionBoard =
    ActionBoard []


newAvailableRooms : List (RoomTile Resources)
newAvailableRooms =
    [tileWarehouse,
    tileAltareSacrificale,
    tileBancarella,
    tileCameraSegreta,
    tileCavaInEspansione,
    tileDeposito,
    tileFiliera,
    tileForno,
    tileGoldMine,
    tileOfficina,
    tileLuxuryRoom,
    tileStanzaDiSnodo,
    tileTesoreria,
    tilePlaceholder,
    tilePlaceholder,
    tilePlaceholder,
    tilePlaceholder,
    tilePlaceholder,

    tileShelf, tileSpinningWheel, tileMacina, tileSalotto, tileTunnel, tileFoodCorner]


subscriptions : Game -> Sub Msg
subscriptions model =
    Sub.none


initPlayerBoard: Game -> Int -> Game
initPlayerBoard ({activePlayer, waitingPlayer, availableRooms } as model) index =
    let
        arr = Array.fromList availableRooms
        room = safeGet index arr tilePlaceholder
    in
    if List.length model.activePlayer.rooms == 9 then
        { model | waitingPlayer = { waitingPlayer | rooms = room :: waitingPlayer.rooms }}
    else
        { model | activePlayer = { activePlayer | rooms = room :: activePlayer.rooms }}


safeGet index arr def =
    let
        item = Array.get index arr
    in
        case item of
            Just val -> val
            Nothing -> def

update : Msg -> Game -> ( Game, Cmd Msg )
update msg ({activePlayer} as model) =
    case msg of
        InitPlayerBoard index ->
            if List.length model.waitingPlayer.rooms == 9 then
                (model, Cmd.none)
            else
                (initPlayerBoard model index, (initRandom index))

        DoAction action ->
            if action.isDoable model.activePlayer.resources then
                ( { model | activePlayer = { activePlayer | resources = action.do model.activePlayer.resources} }, Cmd.none )

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
        Html.map remap (viewBoard game.activePlayer),
        viewAvailableRooms game.activePlayer.resources game.availableRooms,
        Html.map remap (viewBoard game.waitingPlayer)
    ]


viewAvailableRooms: Resources -> List (RoomTile Resources) -> Html Msg
viewAvailableRooms resources rooms =
    div [class "availablerooms"] (List.map (viewTile resources) rooms)

viewTile room resources =
    Html.map remap (Tiles.viewTile room resources)

remap: Tiles.Msg -> Msg
remap html =
    case html of
        Tiles.DoAction action ->
            DoAction action


