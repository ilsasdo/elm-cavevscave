module Tiles exposing (..)

import Array exposing (Array)
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Resources exposing (Resources, noWalls, priceFree, priceGold, priceStone, priceWood)
import Walls exposing (Wall(..), Walls)


type TileStatus
    = Available
    | Active
    | Rock
    | Empty

type TileType
    = Orange
    | Blue
    | Gray

type alias Tile =
    { title : String
    , tileType : TileType
    , status : TileStatus
    , score : Int
    , src : String
    , price : Resources
    , walls : Walls
    , actions : List Action
    }


type alias Action =
    { classes : String
    , available : Bool
    , isDoable : Resources -> Bool
    , do : Resources -> Resources
    , subphase : Maybe Subphase
    , disableActions : List Int
    }


type Msg
    = SelectRoomTile Tile
    | DoAction Tile Action
    | SelectWall Int


type Subphase
    = Escavate1
    | Escavate2
    | Furnish
    | PlaceRoom Tile
    | BuildWall
    | DestroyWall
    | EscavateThroughWall
    | Activate1
    | Activate2
    | Activate3


subphaseToString : Maybe Subphase -> String
subphaseToString subphase =
    case subphase of
        Nothing ->
            ""

        Just Escavate1 ->
            "Escavate 1"

        Just Escavate2 ->
            "Escavate 2"

        Just Furnish ->
            "Furnish"

        Just (PlaceRoom tile) ->
            "PlaceRoom " ++ tile.title

        Just BuildWall ->
            "Build a Wall"

        Just DestroyWall ->
            "Destroy a Wall"

        Just EscavateThroughWall ->
            "Escavate through a Wall"

        Just Activate1 ->
            "Activate a Room 1"

        Just Activate2 ->
            "Activate a Room 2"

        Just Activate3 ->
            "Activate a Room 3"


updateStatus tile status tiles =
    List.map
        (\t ->
            if t.title == tile.title then
                { t | status = status }

            else
                t
        )
        tiles


updateWalls : Array Wall -> List Tile -> List Tile
updateWalls walls tiles =
    List.indexedMap (updateTileWalls walls) tiles



{-

   |Walls| and Tiles indexes

   |================|
   |  0   |0|   1   |
   |==|1|======|2|==|
   |  2   |3|   3   |
   |==|4|======|5|==|
   |  4   |6|   5   |
   |==|7|======|8|==|
   |  6   |9|   7   |
   |==|10|=====|11|============|
   |   8  |12|  9   |13|   10  |
   |===========================|
-}


updateTileWalls : Array Wall -> Int -> Tile -> Tile
updateTileWalls walls index tile =
    if tile.status /= Empty then
        tile

    else
        case index of
            0 ->
                { tile | walls = Walls Placed (Walls.get 0 walls) (Walls.get 1 walls) Placed }

            1 ->
                { tile | walls = Walls Placed Placed (Walls.get 2 walls) (Walls.get 0 walls) }

            2 ->
                { tile | walls = Walls (Walls.get 1 walls) (Walls.get 3 walls) (Walls.get 4 walls) Placed }

            3 ->
                { tile | walls = Walls (Walls.get 2 walls) Placed (Walls.get 5 walls) (Walls.get 3 walls) }

            4 ->
                { tile | walls = Walls (Walls.get 4 walls) (Walls.get 6 walls) (Walls.get 7 walls) Placed }

            5 ->
                { tile | walls = Walls (Walls.get 5 walls) Placed (Walls.get 8 walls) (Walls.get 6 walls) }

            6 ->
                { tile | walls = Walls (Walls.get 7 walls) (Walls.get 9 walls) (Walls.get 10 walls) Placed }

            7 ->
                { tile | walls = Walls (Walls.get 8 walls) Placed (Walls.get 9 walls) (Walls.get 11 walls) }

            8 ->
                { tile | walls = Walls (Walls.get 10 walls) (Walls.get 12 walls) Placed Placed }

            9 ->
                { tile | walls = Walls (Walls.get 11 walls) (Walls.get 13 walls) Placed (Walls.get 12 walls) }

            10 ->
                { tile | walls = Walls Placed Placed Placed (Walls.get 13 walls) }

            _ ->
                tile


viewTile : List (Attribute Msg) -> Resources -> Tile -> Html Msg
viewTile attributes resources tile =
    div attributes
        [ case tile.status of
            Active ->
                div
                    [ style "background-image" ("url(" ++ tile.src ++ ")")
                    , class "tile"
                    ]
                    (List.indexedMap (\index -> \action -> viewAction tile resources action index) tile.actions)

            Available ->
                div
                    [ style "background-image" ("url(" ++ tile.src ++ ")")
                    , class "tile"
                    ]
                    []

            Empty ->
                div [ class "tile empty" ] []

            Rock ->
                div [ class "tile hidden" ] []
        ]


viewAction : Tile -> Resources -> Action -> Int -> Html Msg
viewAction tile resources action index =
    if action.available && action.isDoable resources then
        div [ class ("action doable " ++ action.classes), onClick (DoAction tile action) ] []

    else
        div [ class ("action notdoable " ++ action.classes) ] []


consumeAction : Tile -> Action -> Tile
consumeAction tile action =
    { tile
        | actions =
            tile.actions
                |> List.indexedMap
                    (\index ->
                        \a ->
                            if List.member index action.disableActions then
                                { a | available = False }

                            else
                                a
                    )
    }

setStatus: TileStatus -> Tile -> Tile
setStatus status tile =
    {tile | status = status }

---------------------------------------------
-------------Action Tiles--------------------
---------------------------------------------

tileFreeAction : Tile
tileFreeAction =
    Tile "Free Action"
        Gray
        Active
        0
        "none"
        priceFree
        noWalls
        [ firstAction (require ((<) 0) .emmer) (\r -> r |> addFood 1 |> addEmmer -1) Nothing []
        , secondAction (require ((<) 0) .flax) (\r -> r |> addFood 1 |> addFlax -1) Nothing []
        , thirdAction (require ((<) 0) .gold) (\r -> r |> addFood 1 |> addGold -1) Nothing []]

tileLavoriDomestici : Tile
tileLavoriDomestici =
    Tile "Lavori Domestici"
        Gray
        Available
        3
        "assets/img/rounds/lavori_domestici.jpg"
        priceFree
        noWalls
        [ topAction (\r -> r.food > r.actions) (\r -> r |> addFood r.actions) (Just Furnish) [ 0 ]
        , bottomLeftAction (require ((<=) 5) .food) (addFood -5) (Just Furnish) [ 1, 2 ]
        , bottomRightAction (require ((<=) 1) .gold) (addGold -1) (Just Furnish) [ 1, 2 ]
        ]


tileColtivare : Tile
tileColtivare =
    Tile "Coltivare" Gray
        Available
        2
        "assets/img/rounds/coltivare.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (\r -> r) (Just Activate1) [ 0 ]
        , bottomAction alwaysDoable (\r -> r |> addEmmer 2 |> addFlax 1) Nothing [ 1 ]
        ]


tileSottobosco : Tile
tileSottobosco =
    Tile "Sottobosco" Gray
        Available
        1
        "assets/img/rounds/sottobosco.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (\r -> r) (Just Activate1) [ 0 ]
        , bottomAction alwaysDoable (addWood 2) Nothing [ 1 ]
        ]


tileScavare : Tile
tileScavare =
    Tile "Scavare" Gray
        Available
        4
        "assets/img/rounds/scavare.jpg"
        priceFree
        noWalls
        [ topLeftAction alwaysDoable (\r -> r) (Just Escavate1) [ 0, 1 ]
        , topRightAction (require ((<=) 2) .food) (addFood -2) (Just Escavate2) [ 0, 1 ]
        , bottomAction alwaysDoable (addStone 1) Nothing [ 2 ]
        ]


tileArredare : Tile
tileArredare =
    Tile "Arredare" Gray
        Rock
        6
        "assets/img/rounds/arredare.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (addFood 1) Nothing [ 0 ]
        , bottomAction (\r -> r.food > r.actions) (\r -> r |> addFood r.actions) (Just Furnish) [ 1 ]
        ]


tileCostruireUnMuro : Tile
tileCostruireUnMuro =
    Tile "Costrurire un Muro" Gray
        Rock
        7
        "assets/img/rounds/costruire_un_muro.jpg"
        priceFree
        noWalls
        [ topLeftAction alwaysDoable (\r -> r) (Just Activate1) [ 0 ]
        , thirdAction alwaysDoable (addWood 1) Nothing [ 1, 2 ]
        , fourthAction alwaysDoable (addStone 1) Nothing [ 1, 2 ]
        , bottomAction (require ((<) 0) .availableWalls) (\r -> r) (Just BuildWall) [ 3 ]
        ]


tileMinare : Tile
tileMinare =
    Tile "Minare" Gray
        Rock
        5
        "assets/img/rounds/minare.jpg"
        priceFree
        noWalls
        [ leftAction alwaysDoable (\r -> r) (Just Activate2) [ 0, 1 ]
        , rightAction alwaysDoable (\r -> r) (Just EscavateThroughWall) [ 0, 1 ]
        ]



--TODO: available only if there are walls to destroy


tileDemolireUnMuro : Tile
tileDemolireUnMuro =
    Tile "Demolire un Muro" Gray
        Rock
        0
        "assets/img/rounds/demolire_un_muro.jpg"
        priceFree
        noWalls
        -- TODO: resources should be update after wall choice
        [ fullAction alwaysDoable (\r -> r |> addStone 2 |> addFood 3 |> addGold 1) (Just DestroyWall) [ 0 ] ]


tileEspansione : Tile
tileEspansione =
    Tile "Espansione" Gray
        Rock
        10
        "assets/img/rounds/espansione.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (\r -> r) (Just Escavate1) [ 0 ]
        , bottomLeftAction (require ((<=) 5) .food) (addFood -5) (Just Furnish) [ 1, 2 ]
        , bottomRightAction (require ((<=) 1) .gold) (addGold -1) (Just Furnish) [ 1, 2 ]
        ]


tileSpedizione : Tile
tileSpedizione =
    Tile "Spedizione" Gray
        Rock
        9
        "assets/img/rounds/spedizione.jpg"
        priceFree
        noWalls
        [ firstAction (require ((<=) 5) .wood) (\r -> r |> addWood -5 |> addGold 5) Nothing [ 0, 1, 2 ]
        , secondAction (require ((<=) 5) .stone) (\r -> r |> addStone -5 |> addGold 5) Nothing [ 0, 1, 2 ]
        , rightAction alwaysDoable (\r -> r) (Just Activate3) [ 0, 1, 2 ]
        ]


tilePerforare : Tile
tilePerforare =
    Tile "Perforare" Gray
        Rock
        8
        "assets/img/rounds/perforare.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (\r -> r) (Just Activate1) [ 0 ]
        , bottomAction (\r -> require ((<) r.opponentsGold) .gold r) (\r -> r) (Just Escavate1) [ 1 ]
        ]


tileRinnovare : Tile
tileRinnovare =
    Tile "Rinnovare" Gray
        Rock
        11
        "assets/img/rounds/rinnovare.jpg"
        priceFree
        noWalls
        [ topAction (require ((<) 0) .availableWalls) (\r -> r) (Just BuildWall) [ 0 ]
        , bottomAction alwaysDoable (\r -> r) (Just Furnish) [ 1 ]
        ]



-------------------------------------------
-------------EQUIPMENTS--------------------
-------------------------------------------


tileRock : Tile
tileRock =
    Tile "Rock Tile" Gray
        Rock
        0
        ""
        priceFree
        (Walls None None None None)
        []


tileEmpty : Tile
tileEmpty =
    Tile "Empty Tile" Gray
        Empty
        0
        ""
        priceFree
        (Walls None None None Placed)
        []



-- TODO: Handle Equipment Events


tileSotterraneo : Tile
tileSotterraneo =
    Tile "Sotterraneo" Blue
        Rock
        11
        "assets/img/equipments/sotterraneo.jpg"
        (priceFree |> priceGold 4 |> priceStone 3)
        (Walls Placed Placed Placed Placed)
        []


tileLavorareIlLino : Tile
tileLavorareIlLino =
    Tile "Lavorare il Lino" Blue
        Rock
        3
        "assets/img/equipments/lavorare_il_lino.jpg"
        (priceFree |> priceStone 1)
        (Walls Placed Optional Walls.None Placed)
        []


tileEquipaggiamenti : Tile
tileEquipaggiamenti =
    Tile "Equipaggiamenti" Blue
        Rock
        3
        "assets/img/equipments/equipaggiamenti.jpg"
        (priceFree |> priceWood 2)
        (Walls Placed Walls.None Walls.None Optional)
        []


tileDepositoDiLegna : Tile
tileDepositoDiLegna =
    Tile "Deposito di Legna" Blue
        Rock
        2
        "assets/img/equipments/deposito_di_legna.jpg"
        (priceFree |> priceStone 1)
        (Walls Placed Walls.None Walls.None Placed)
        []


tileAnalisiTerritoriale : Tile
tileAnalisiTerritoriale =
    Tile "Analisi Territoriale" Blue
        Rock
        5
        "assets/img/equipments/deposito_di_legna.jpg"
        priceFree
        (Walls Placed Walls.None Walls.None Optional)
        []



-------------------------------------------
-------------ROOM TILES--------------------
-------------------------------------------


tileCaveEntrance : Tile
tileCaveEntrance =
    Tile "Entrata della Cava" Orange
        Available
        0
        "assets/img/rooms/entrata_della_cava.jpg"
        priceFree
        noWalls
        [ firstAction alwaysDoable (addWood 1) Nothing [ 0, 1, 2, 3 ]
        , secondAction alwaysDoable (addStone 1) Nothing [ 0, 1, 2, 3 ]
        , thirdAction alwaysDoable (addEmmer 1) Nothing [ 0, 1, 2, 3 ]
        , fourthAction alwaysDoable (addFlax 1) Nothing [ 0, 1, 2, 3 ]
        ]


tileWarehouse : Tile
tileWarehouse =
    Tile "Magazzino" Orange
        Rock
        2
        "assets/img/rooms/magazzino.jpg"
        (priceFree |> priceWood 2)
        (Walls Placed Optional Walls.None Optional)
        [ fullAction (require ((<=) 2) .food)
            (\res ->
                res
                    |> addFood -2
                    |> addWood 1
                    |> addStone 1
                    |> addFlax 1
                    |> addEmmer 1
            )
            Nothing
            [ 0 ]
        ]


tileShelf : Tile
tileShelf =
    Tile "Shelf" Orange
        Available
        3
        "assets/img/rooms/scaffale.jpg"
        (priceFree |> priceWood 1)
        (Walls Placed Walls.None Walls.None Walls.None)
        [ firstAction (require ((>) 2) .wood) (topWood 2) Nothing [ 0, 1, 2, 3 ]
        , secondAction (require ((>) 2) .stone) (topStone 2) Nothing [ 0, 1, 2, 3 ]
        , thirdAction (require ((>) 2) .emmer) (topEmmer 2) Nothing [ 0, 1, 2, 3 ]
        , fourthAction (require ((>) 2) .flax) (topFlax 2) Nothing [ 0, 1, 2, 3 ]
        ]


tileFoodCorner : Tile
tileFoodCorner =
    Tile "Angolo del Cibo" Orange
        Available
        3
        "assets/img/rooms/angolo_del_cibo.jpg"
        (priceFree |> priceStone 1)
        (Walls Placed Walls.None Walls.None Placed)
        [ fullAction (require ((>) 3) .food) (topFood 3) Nothing [ 0 ] ]


tileSpinningWheel : Tile
tileSpinningWheel =
    Tile "Filatoio" Orange
        Available
        4
        "assets/img/rooms/filatoio.jpg"
        (priceFree |> priceWood 1)
        (Walls Placed Walls.None Walls.None Walls.None)
        [ leftAction (require ((<=) 1) .flax) (\res -> res |> addFlax -1 |> addGold 1) Nothing [ 0, 1 ]
        , rightAction (require ((<=) 3) .flax) (\res -> res |> addFlax -3 |> addGold 2) Nothing [ 0, 1 ]
        ]


tileTunnel : Tile
tileTunnel =
    Tile "Tunnel" Orange
        Available
        3
        "assets/img/rooms/tunnel.jpg"
        (priceFree |> priceWood 1)
        (Walls Walls.None Placed Walls.None Placed)
        [ topAction alwaysDoable (addFood 2) Nothing [ 0 ]
        , bottomAction (require ((>) 3) .stone) (\resources -> resources |> addStone 1 |> minStone 3) Nothing [ 1 ]
        ]


tileAltareSacrificale : Tile
tileAltareSacrificale =
    Tile "Altare Sacrificale" Orange
        Rock
        7
        "assets/img/rooms/altare_sacrificale.jpg"
        (priceFree |> priceStone 4)
        (Walls Placed Optional Walls.None Optional)
        [ fullAction
            (\res ->
                require ((<=) 1) .wood res
                    && require ((<=) 1) .emmer res
                    && require ((<=) 1) .flax res
                    && require ((<=) 1) .food res
            )
            (\res -> res |> addEmmer -1 |> addWood -1 |> addFlax -1 |> addFood -1 |> addGold 3)
            Nothing
            [ 0 ]
        ]


tileBancarella : Tile
tileBancarella =
    Tile "Bancarella" Orange
        Rock
        6
        "assets/img/rooms/bancarella.jpg"
        (priceFree |> priceWood 1 |> priceGold 1)
        (Walls Placed Optional Walls.None Optional)
        [ leftAction (require ((<=) 5) .emmer) (\res -> res |> addEmmer -5 |> addGold 4) Nothing [ 0, 1 ]
        , rightAction (require ((<=) 5) .flax) (\res -> res |> addFlax -5 |> addGold 4) Nothing [ 0, 1 ]
        ]


tileCameraSegreta : Tile
tileCameraSegreta =
    Tile "Camera Segreta" Orange
        Rock
        8
        "assets/img/rooms/camera_segreta.jpg"
        (priceFree |> priceWood 2 |> priceStone 1)
        (Walls Placed Placed Placed Placed)
        [ leftAction alwaysDoable (\res -> res |> addFlax 3) Nothing [ 0, 1 ]
        , rightAction alwaysDoable (\res -> res |> addGold 1) Nothing [ 0, 1 ]
        ]


tileCavaInEspansione : Tile
tileCavaInEspansione =
    Tile "Cava in Espansione" Orange
        Rock
        8
        "assets/img/rooms/cava_in_espansione.jpg"
        (priceFree |> priceWood 1 |> priceStone 3)
        (Walls Placed Placed Walls.None Placed)
        [ fullAction (require ((<=) 1) .gold) (\res -> res) Nothing [ 0 ] ]


tileDeposito : Tile
tileDeposito =
    Tile "Deposito" Orange
        Rock
        6
        "assets/img/rooms/deposito.jpg"
        (priceFree |> priceWood 2 |> priceGold 1)
        (Walls Placed Walls.None Walls.None Placed)
        [ fullAction alwaysDoable (\res -> res |> addEmmer 1 |> addFlax 1 |> addFood 1) Nothing [ 0 ] ]


tileFiliera : Tile
tileFiliera =
    Tile "Filiera" Orange
        Rock
        5
        "assets/img/rooms/filiera.jpg"
        (priceFree |> priceWood 2)
        (Walls Placed Walls.None Walls.None Placed)
        [ fullAction (require ((<=) 2) .flax) (\res -> res |> addFlax -2 |> addGold 2 |> addFood 2) Nothing [ 0 ] ]


tileForno : Tile
tileForno =
    Tile "Forno" Orange
        Rock
        6
        "assets/img/rooms/forno.jpg"
        (priceFree |> priceWood 1 |> priceStone 2)
        (Walls Placed Placed Walls.None Placed)
        [ leftAction (require ((<=) 2) .emmer) (\res -> res |> addEmmer -2 |> addFood 4 |> addGold 1) Nothing [ 0, 1 ]
        , rightAction (require ((<=) 3) .emmer) (\res -> res |> addEmmer -3 |> addFood 4 |> addGold 2) Nothing [ 0, 1 ]
        ]


tileMacina : Tile
tileMacina =
    Tile "Macina" Orange
        Available
        4
        "assets/img/rooms/macina.jpg"
        (priceFree |> priceStone 1)
        (Walls Placed Walls.None Walls.None Optional)
        [ leftAction (require ((<=) 2) .emmer) (\res -> res |> addEmmer -1 |> addFood 3) Nothing [ 0, 1 ]
        , rightAction (require ((<=) 3) .emmer) (\res -> res |> addEmmer -4 |> addFood 7) Nothing [ 0, 1 ]
        ]


tileGoldMine : Tile
tileGoldMine =
    Tile "Miniera d'Oro" Orange
        Rock
        9
        "assets/img/rooms/miniera_d_oro.jpg"
        (priceFree |> priceGold 5)
        (Walls Placed Optional Walls.None Placed)
        [ fullAction alwaysDoable (\res -> res |> addGold 1 |> addStone 1) Nothing [ 0 ] ]


tileOfficina : Tile
tileOfficina =
    Tile "Officina" Orange
        Rock
        5
        "assets/img/rooms/officina.jpg"
        (priceFree |> priceWood 1 |> priceStone 2)
        (Walls Placed Optional Walls.None Placed)
        [ fullAction
            (\res ->
                require ((<=) 1) .flax res
                    && require ((<=) 2) .food res
            )
            (\res -> res |> addWood -2 |> addFlax -1 |> addGold 3)
            Nothing
            [ 0 ]
        ]


tileSalotto : Tile
tileSalotto =
    Tile "Salotto" Orange
        Available
        6
        "assets/img/rooms/salotto.jpg"
        (priceFree |> priceStone 1 |> priceGold 1)
        (Walls Placed Placed Walls.None Placed)
        [ fullAction alwaysDoable (\res -> res |> topWood 1 |> topWood 1 |> topEmmer 1 |> topFlax 1 |> topFood 1 |> topGold 1) Nothing [ 0 ] ]


tileLuxuryRoom : Tile
tileLuxuryRoom =
    Tile "Stanza di Lusso" Orange
        Rock
        12
        "assets/img/rooms/stanza_di_lusso.jpg"
        (priceFree |> priceGold 7)
        (Walls Placed Placed Optional Placed)
        [ fullAction alwaysDoable (\res -> res |> addGold 1 |> addFlax 1) Nothing [ 0 ] ]



-- TODO: the user should choose three resources to spend


tileStanzaDiSnodo : Tile
tileStanzaDiSnodo =
    Tile "Stanza di Snodo" Orange
        Rock
        6
        "assets/img/rooms/stanza_di_snodo.jpg"
        (priceFree |> priceWood 2)
        (Walls Placed Placed Optional Placed)
        [ fullAction Resources.atLeastThreeResources (\res -> res |> addGold 2) Nothing [ 0 ] ]


tileTesoreria : Tile
tileTesoreria =
    Tile "Tesoreria" Orange
        Rock
        10
        "assets/img/rooms/tesoreria.jpg"
        (priceFree |> priceGold 3)
        (Walls Placed Placed Placed Placed)
        [ fullAction (require ((<=) 3) .gold) (\res -> res |> addGold -3 |> addGold 4 |> addFood 1) Nothing [ 0 ] ]


require : (Int -> Bool) -> (Resources -> Int) -> Resources -> Bool
require condition getter resources =
    condition (getter resources)


topWood : Int -> Resources -> Resources
topWood qty resources =
    { resources | wood = Basics.max resources.wood qty }


topFood : Int -> Resources -> Resources
topFood qty resources =
    { resources | food = Basics.max resources.food qty }


topStone : Int -> Resources -> Resources
topStone qty resources =
    { resources | stone = Basics.max resources.stone qty }


topGold : Int -> Resources -> Resources
topGold qty resources =
    { resources | gold = Basics.max resources.gold qty }


minStone : Int -> Resources -> Resources
minStone qty resources =
    { resources | stone = Basics.min resources.stone qty }


topFlax : Int -> Resources -> Resources
topFlax qty resources =
    { resources | flax = Basics.max resources.flax qty }


topEmmer : Int -> Resources -> Resources
topEmmer qty resources =
    { resources | emmer = Basics.max resources.emmer qty }


addWood : Int -> Resources -> Resources
addWood qty resources =
    { resources | wood = min 9 (resources.wood + qty) }


addStone : Int -> Resources -> Resources
addStone qty resources =
    { resources | stone = min 9 (resources.stone + qty) }


addFlax : Int -> Resources -> Resources
addFlax qty resources =
    { resources | flax = min 9 (resources.flax + qty) }


addEmmer : Int -> Resources -> Resources
addEmmer qty resources =
    { resources | emmer = min 9 (resources.emmer + qty) }


addFood : Int -> Resources -> Resources
addFood qty resources =
    { resources | food = min 9 (resources.food + qty) }


addGold : Int -> Resources -> Resources
addGold qty resources =
    { resources | gold = min 19 (resources.gold + qty) }


alwaysDoable : Resources -> Bool
alwaysDoable resources =
    True


firstAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> List Int -> Action
firstAction isDoable do subphase disableActions =
    Action "first" True isDoable do subphase disableActions


secondAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> List Int -> Action
secondAction isDoable do subphase disableActions =
    Action "second" True isDoable do subphase disableActions


thirdAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> List Int -> Action
thirdAction isDoable do subphase disableActions =
    Action "third" True isDoable do subphase disableActions


fourthAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> List Int -> Action
fourthAction isDoable do subphase disableActions =
    Action "fourth" True isDoable do subphase disableActions


leftAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> List Int -> Action
leftAction isDoable do subphase disableActions =
    Action "left" True isDoable do subphase disableActions


rightAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> List Int -> Action
rightAction isDoable do subphase disableActions =
    Action "right" True isDoable do subphase disableActions


topAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> List Int -> Action
topAction isDoable do subphase disableActions =
    Action "top" True isDoable do subphase disableActions


topLeftAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> List Int -> Action
topLeftAction isDoable do subphase disableActions =
    Action "topleft" True isDoable do subphase disableActions


topRightAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> List Int -> Action
topRightAction isDoable do subphase disableActions =
    Action "topright" True isDoable do subphase disableActions


bottomLeftAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> List Int -> Action
bottomLeftAction isDoable do subphase disableActions =
    Action "bottomleft" True isDoable do subphase disableActions


bottomRightAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> List Int -> Action
bottomRightAction isDoable do subphase disableActions =
    Action "bottomright" True isDoable do subphase disableActions


bottomAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> List Int -> Action
bottomAction isDoable do subphase disableActions =
    Action "bottom" True isDoable do subphase disableActions


fullAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> List Int -> Action
fullAction isDoable do subphase disableActions =
    Action "full" True isDoable do subphase disableActions
