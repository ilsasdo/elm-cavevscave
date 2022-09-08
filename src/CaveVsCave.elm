module CaveVsCave exposing (main)

import Browser
import Debug exposing (toString)
import Html exposing (Html, div, img, p, text)
import Html.Attributes exposing (class, coords, name, shape, src, style, usemap)
import Html.Events exposing (onClick)


type alias ActionBoard =
    { actionTiles : List ActionTile
    }


type alias ActionTile =
    { hidden : Bool
    , actions : List Action
    }


type alias Action =
    { point : ( Int, Int )
    , size : ( Int, Int )
    , isDoable : CaveBoard -> Bool
    , do : CaveBoard -> CaveBoard
    }


type Actions
    = Actions (List Action)


type alias RoomTile =
    { title : String
    , score : Int
    , src : String
    , price : Resources
    , requiredWalls : Walls
    , actions : Actions
    }


type Wall
    = Fixed
    | Placed
    | Required
    | Optional
    | None


type alias Walls =
    { north : Wall
    , east : Wall
    , south : Wall
    , west : Wall
    }


type alias Cave =
    { bonus : Bool
    , tile : RoomTile
    , walls : Walls
    }


type alias CaveBoard =
    { cave : List Cave
    , resources : Resources
    }


type alias Resources =
    { gold : Int
    , wood : Int
    , stone : Int
    , emmer : Int
    , flax : Int
    , food : Int
    }


type alias Game =
    { currentPlayer : CaveBoard
    , waitingPlayer : CaveBoard
    , turn : Int
    , actionBoard : ActionBoard
    , availableRooms : List RoomTile
    }


type Msg
    = DoAction Action


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


init : () -> ( Game, Cmd Msg )
init _ =
    ( Game (newBoard ()) (newBoard ()) 1 (newActionBoard ()) (newAvailableRooms ()), Cmd.none )


newBoard : () -> CaveBoard
newBoard _ =
    CaveBoard [] (Resources 1 1 1 1 1 1)


newActionBoard : () -> ActionBoard
newActionBoard _ =
    ActionBoard []


newAvailableRooms : () -> List RoomTile
newAvailableRooms _ =
    []


subscriptions : Game -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Game -> ( Game, Cmd Msg )
update msg model =
    case msg of
        DoAction action ->
            if action.isDoable model.currentPlayer then
                ( { model | currentPlayer = action.do model.currentPlayer }, Cmd.none )

            else
                ( model, Cmd.none )


view : Game -> Html Msg
view model =
    div []
        [ viewTile model.currentPlayer tileShelf
        , viewTile model.currentPlayer tileCaveEntrance
        , viewTile model.currentPlayer tileWarehouse
        , viewTile model.currentPlayer tileFoodCorner
        , viewTile model.currentPlayer tileSpinningWheel
        , viewTile model.currentPlayer tileTunnel
        , viewBoard model.currentPlayer
        ]


viewBoard : CaveBoard -> Html Msg
viewBoard board =
    div []
        [ p [] [ text ("gold: " ++ toString board.resources.gold) ]
        , p [] [ text ("wood: " ++ toString board.resources.wood) ]
        , p [] [ text ("emmer: " ++ toString board.resources.emmer) ]
        , p [] [ text ("flax: " ++ toString board.resources.flax) ]
        , p [] [ text ("stone: " ++ toString board.resources.stone) ]
        , p [] [ text ("food: " ++ toString board.resources.food) ]
        ]


viewTile : CaveBoard -> RoomTile -> Html Msg
viewTile board tile =
    div [ style "background-image" ("url(" ++ tile.src ++ ")"), class "tile" ] (viewActions board tile.actions)


viewActions : CaveBoard -> Actions -> List (Html Msg)
viewActions board (Actions actions) =
    List.map (viewAction board) actions


viewAction : CaveBoard -> Action -> Html Msg
viewAction board action =
    let
        ( x, y ) =
            action.point

        ( width, height ) =
            action.size

        doable =
            action.isDoable board
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



------------------------------------------------------
----------------------- TILES ------------------------
------------------------------------------------------

middleY = 133
offsetY = -15
firstX = 50
secondX = 50 + 35
thirdX = 50 + 35 + 35
fourthX = 50 + 35 + 35 + 35
mediumIcon = (40, 40)
littleIcon = (30, 30)

topLeft = (25, 135)
topRight = (125, 135)

upHalf = (10, 90)
lowerHalf = (10, 135)

halfTile = (190, 40)

tileCaveEntrance : RoomTile
tileCaveEntrance =
    RoomTile "Cave Entrance"
        0
        "assets/img/entrata_della_cava.jpg"
        priceFree
        noWalls
        (Actions
            [ Action ( firstX, middleY ) littleIcon alwaysDoable (addWood 1)
            , Action ( secondX, middleY ) littleIcon alwaysDoable (addStone 1)
            , Action ( thirdX, middleY ) littleIcon alwaysDoable (addEmmer 1)
            , Action ( fourthX, middleY ) littleIcon alwaysDoable (addFlax 1)
            ]
        )


tileWarehouse : RoomTile
tileWarehouse =
    RoomTile "Warehouse"
        2
        "assets/img/magazzino.jpg"
        (priceFree |> priceWood 2)
        (Walls Required Optional None Optional)
        (Actions
            [ Action ( 20, middleY )
                mediumIcon
                (require .food (gt 2))
                (\board ->
                    board
                        |> addFood -2
                        |> addWood 1
                        |> addStone 1
                        |> addFlax 1
                        |> addEmmer 1
                )
            ]
        )


tileShelf : RoomTile
tileShelf =
    RoomTile "Shelf"
        3
        "assets/img/scaffale.jpg"
        (priceFree |> priceWood 2)
        (Walls Required None None None)
        (Actions
            [ Action ( firstX,  middleY + offsetY ) littleIcon (require .wood (lt 2)) (topWood 2)
            , Action ( secondX, middleY + offsetY ) littleIcon (require .stone (lt 2)) (topStone 2)
            , Action ( thirdX,  middleY + offsetY ) littleIcon (require .emmer (lt 2)) (topEmmer 2)
            , Action ( fourthX, middleY + offsetY ) littleIcon (require .flax (lt 2)) (topFlax 2)
            ]
        )


-- TODO: these two actions are not mutually exclusive

tileTunnel: RoomTile
tileTunnel =
    RoomTile "Tunnel"
        3
        "assets/img/tunnel.jpg"
        (priceFree |> priceWood 1)
        (Walls None Required None Required)
        (Actions
            [ Action upHalf halfTile alwaysDoable (addFood 2)
            , Action lowerHalf halfTile alwaysDoable (\board -> board |> addStone 1 |> minStone 3)
            ]
        )


tileFoodCorner: RoomTile
tileFoodCorner =
    RoomTile "Food Corner"
        3
        "assets/img/angolo_del_cibo.jpg"
        (priceFree |> priceStone 1)
        (Walls Required None None Required)
        (Actions
            [ Action ( 20, 20 ) ( 40, 20 ) alwaysDoable (topFood 3)
            ]
        )


tileSpinningWheel: RoomTile
tileSpinningWheel =
    RoomTile "Spinning Wheel"
        4
        "assets/img/filatoio.jpg"
        (priceFree |> priceWood 1)
        (Walls Required None None None)
        (Actions
            [ Action topLeft  mediumIcon (require .flax (gt 1)) (\board -> board |> addFlax -1 |> addGold 1)
            , Action topRight mediumIcon (require .flax (gt 3)) (\board -> board |> addFlax -3 |> addGold 2)
            ]
        )


require: (Resources -> Int) -> (Int -> Bool) -> CaveBoard -> Bool
require resource condition board =
    condition (resource board.resources)


topWood : Int -> CaveBoard -> CaveBoard
topWood qty ({ resources } as board) =
    { board | resources = { resources | wood = Basics.max resources.wood qty } }


topFood : Int -> CaveBoard -> CaveBoard
topFood qty ({ resources } as board) =
    { board | resources = { resources | food = Basics.max resources.food qty } }


topStone : Int -> CaveBoard -> CaveBoard
topStone qty ({ resources } as board) =
    { board | resources = { resources | stone = Basics.max resources.stone qty } }


minStone : Int -> CaveBoard -> CaveBoard
minStone qty ({ resources } as board) =
    { board | resources = { resources | stone = Basics.min resources.stone qty } }


topFlax : Int -> CaveBoard -> CaveBoard
topFlax qty ({ resources } as board) =
    { board | resources = { resources | flax = Basics.max resources.flax qty } }


topEmmer : Int -> CaveBoard -> CaveBoard
topEmmer qty ({ resources } as board) =
    { board | resources = { resources | emmer = Basics.max resources.emmer qty } }


addWood : Int -> CaveBoard -> CaveBoard
addWood qty ({ resources } as board) =
    { board | resources = { resources | wood = resources.wood + qty } }


addStone : Int -> CaveBoard -> CaveBoard
addStone qty ({ resources } as board) =
    { board | resources = { resources | stone = resources.stone + qty } }


addFlax : Int -> CaveBoard -> CaveBoard
addFlax qty ({ resources } as board) =
    { board | resources = { resources | flax = resources.flax + qty } }


addEmmer : Int -> CaveBoard -> CaveBoard
addEmmer qty ({ resources } as board) =
    { board | resources = { resources | emmer = resources.emmer + qty } }


addFood : Int -> CaveBoard -> CaveBoard
addFood qty ({ resources } as board) =
    { board | resources = { resources | food = resources.food + qty } }


addGold : Int -> CaveBoard -> CaveBoard
addGold qty ({ resources } as board) =
    { board | resources = { resources | gold = resources.gold + qty } }


noWalls: Walls
noWalls =
    Walls Optional Optional Optional Optional


priceWood : Int -> Resources -> Resources
priceWood qty resources =
    { resources | wood = resources.wood + qty }


priceStone : Int -> Resources -> Resources
priceStone qty resources =
    { resources | stone = resources.stone + qty }


priceGold : Int -> Resources -> Resources
priceGold qty resources =
    { resources | gold = resources.gold + qty }


priceFree: Resources
priceFree =
    Resources 0 0 0 0 0 0


alwaysDoable : CaveBoard -> Bool
alwaysDoable board =
    True
