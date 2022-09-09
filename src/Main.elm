module Main exposing (main)

import Browser
import CaveBoard exposing (CaveBoard)
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
    CaveBoard (Resources 1 1 1 1 1 1)


newActionBoard : ActionBoard CaveBoard
newActionBoard =
    ActionBoard []


newAvailableRooms : List (RoomTile Resources)
newAvailableRooms =
    []


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
view model =
    div [ style "float" "left"]
        [ viewTile model.currentPlayer tileAltareSacrificale
        , viewTile model.currentPlayer tileFoodCorner
        , viewTile model.currentPlayer tileBancarella
        , viewTile model.currentPlayer tileCameraSegreta
        , viewTile model.currentPlayer tileCavaInEspansione
        , viewTile model.currentPlayer tileDeposito
        , viewTile model.currentPlayer tileCaveEntrance
        , viewTile model.currentPlayer tileSpinningWheel
        , viewTile model.currentPlayer tileFiliera
        , viewTile model.currentPlayer tileForno
        , viewTile model.currentPlayer tileMacina
        , viewTile model.currentPlayer tileWarehouse
        , viewTile model.currentPlayer tileGoldMine
        , viewTile model.currentPlayer tileOfficina
        , viewTile model.currentPlayer tileSalotto
        , viewTile model.currentPlayer tileShelf
        , viewTile model.currentPlayer tileLuxuryRoom
        , viewTile model.currentPlayer tileStanzaDiSnodo
        , viewTile model.currentPlayer tileTesoreria
        , viewTile model.currentPlayer tileTunnel
        , viewBoard model.currentPlayer
        ]


viewBoard : CaveBoard -> Html Msg
viewBoard board =
    div []
        [ p [] [ text ("gold 1111: " ++ toString board.resources.gold) ]
        , p [] [ text ("wood: " ++ toString board.resources.wood) ]
        , p [] [ text ("emmer: " ++ toString board.resources.emmer) ]
        , p [] [ text ("flax: " ++ toString board.resources.flax) ]
        , p [] [ text ("stone: " ++ toString board.resources.stone) ]
        , p [] [ text ("food: " ++ toString board.resources.food) ]
        ]


viewTile : CaveBoard -> (RoomTile Resources) -> Html Msg
viewTile board tile =
    div [ style "background-image" ("url(" ++ tile.src ++ ")"), class "tile" ] (viewActions board tile.actions)


viewActions : CaveBoard -> Actions Resources -> List (Html Msg)
viewActions board (Tiles.Actions actions) =
    List.map (viewAction board) actions


viewAction : CaveBoard -> Action Resources -> Html Msg
viewAction board action =
    let
        ( x, y ) =
            action.point

        ( width, height ) =
            action.size

        doable =
            action.isDoable board.resources
    in
    div
        [ class
            ("action"
                ++ (if doable then
                        " doable"

                    else
                        " notdoable"
                   )
            )
        , style "left" (toString x)
        , style "top" (toString y)
        , style "height" (toString height)
        , style "width" (toString width)
        , onClick (DoAction action)
        ]
        []
