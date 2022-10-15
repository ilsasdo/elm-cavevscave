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


type alias Tile =
    { title : String
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
    }


type Msg
    = SelectRoomTile Tile
    | ActivateTile (Maybe Subphase) Tile Action
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


updateStatus tile status tiles =
    List.map
        (\t ->
            if t.title == tile.title then
                { t | status = status }

            else
                t
        )
        tiles


updateWalls: Array Wall -> List Tile -> List Tile
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
updateTileWalls: Array Wall -> Int -> Tile -> Tile
updateTileWalls walls index tile =
    if tile.status /= Empty then
        tile

    else
        case index of
            0 ->
                {tile | walls = Walls Placed (Walls.get 0 walls) (Walls.get 1 walls) Placed }
            1 ->
                {tile | walls = Walls Placed Placed (Walls.get 2 walls) (Walls.get 0 walls)}
            2 ->
                {tile | walls = Walls (Walls.get 1 walls) (Walls.get 3 walls) (Walls.get 4 walls) Placed}
            3 ->
                {tile | walls = Walls (Walls.get 2 walls) Placed (Walls.get 5 walls) (Walls.get 3 walls)}
            4 ->
                {tile | walls = Walls (Walls.get 4 walls) (Walls.get 6 walls) (Walls.get 7 walls) Placed}
            5 ->
                {tile | walls = Walls (Walls.get 5 walls) Placed (Walls.get 8 walls) (Walls.get 6 walls)}
            6 ->
                {tile | walls = Walls (Walls.get 7 walls) (Walls.get 9 walls) (Walls.get 10 walls) Placed}
            7 ->
                {tile | walls = Walls (Walls.get 8 walls) Placed (Walls.get 9 walls) (Walls.get 11 walls)}
            8 ->
                {tile | walls = Walls (Walls.get 10 walls) (Walls.get 12 walls) Placed Placed}
            9 ->
                {tile | walls = Walls (Walls.get 11 walls) (Walls.get 13 walls) Placed (Walls.get 12 walls)}
            10 ->
                {tile | walls = Walls Placed Placed Placed (Walls.get 13 walls)}
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
    let
        newTile =
            { tile | actions = consumeAction tile.actions index }
    in
    if action.available && action.isDoable resources then
        div [ class ("action doable " ++ action.classes), onClick (DoAction newTile action) ] []

    else
        div [ class ("action notdoable " ++ action.classes) ] []


consumeAction actions index =
    List.indexedMap
        (\i ->
            \a ->
                if i == index then
                    { a | available = False }

                else
                    a
        )
        actions



---------------------------------------------
-------------Action Tiles--------------------
---------------------------------------------


tileLavoriDomestici : Tile
tileLavoriDomestici =
    Tile "Lavori Domestici"
        Available
        3
        "assets/img/rounds/lavori_domestici.jpg"
        priceFree
        noWalls
        [ topAction (\r -> r.food > r.actions) (\r -> r |> addFood r.actions) (Just Furnish)

        -- TODO: left and right action are mutually exclusives
        , bottomLeftAction (require ((<=) 5) .food) (addFood -5) (Just Furnish)
        , bottomRightAction (require ((<=) 1) .gold) (addGold -1) (Just Furnish)
        ]


tileColtivare : Tile
tileColtivare =
    Tile "Coltivare"
        Available
        2
        "assets/img/rounds/coltivare.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (\r -> r) (Just Activate1)
        , bottomAction alwaysDoable (\r -> r |> addEmmer 2 |> addFlax 1) Nothing
        ]


tileSottobosco : Tile
tileSottobosco =
    Tile "Sottobosco"
        Available
        1
        "assets/img/rounds/sottobosco.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (\r -> r) (Just Activate1)
        , bottomAction alwaysDoable (addWood 2) Nothing
        ]


tileScavare : Tile
tileScavare =
    Tile "Scavare"
        Available
        4
        "assets/img/rounds/scavare.jpg"
        priceFree
        noWalls
        [ topLeftAction alwaysDoable (\r -> r) (Just Escavate1)
        , topRightAction (require ((<=) 2) .food) (addFood -2) (Just Escavate2)
        , bottomAction alwaysDoable (addStone 1) Nothing
        ]


tileArredare : Tile
tileArredare =
    Tile "Arredare"
        Rock
        6
        "assets/img/rounds/arredare.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (addFood 1) Nothing
        , bottomAction (\r -> r.food > r.actions) (\r -> r |> addFood r.actions) (Just Furnish)
        ]


tileCostruireUnMuro : Tile
tileCostruireUnMuro =
    Tile "Costrurire un Muro"
        Rock
        7
        "assets/img/rounds/costruire_un_muro.jpg"
        priceFree
        noWalls
        -- TODO: third and fourth action are mutually exclusives
        [ topLeftAction alwaysDoable (\r -> r) (Just Activate1)
        , thirdAction alwaysDoable (addWood 1) Nothing
        , fourthAction alwaysDoable (addStone 1) Nothing
        , bottomAction alwaysDoable (\r -> r) (Just BuildWall) -- TODO: available only if there are walls to build
        ]


tileMinare : Tile
tileMinare =
    Tile "Minare"
        Rock
        5
        "assets/img/rounds/minare.jpg"
        priceFree
        noWalls
        [ leftAction alwaysDoable (\r -> r) (Just Activate2)
        , rightAction alwaysDoable (\r -> r) (Just EscavateThroughWall)
        ]



--TODO: available only if there are walls to destroy


tileDemolireUnMuro : Tile
tileDemolireUnMuro =
    Tile "Demolire un Muro"
        Rock
        0
        "assets/img/rounds/demolire_un_muro.jpg"
        priceFree
        noWalls
        -- TODO: resources should be update after wall choice
        [ fullAction alwaysDoable (\r -> r |> addStone 2 |> addFood 3 |> addGold 1) (Just DestroyWall) ]


tileEspansione : Tile
tileEspansione =
    Tile "Espansione"
        Rock
        10
        "assets/img/rounds/espansione.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (\r -> r) (Just Escavate1)

        -- TODO: left and right action are mutually exclusives
        , bottomLeftAction (require ((<=) 5) .food) (addFood -5) (Just Furnish)
        , bottomRightAction (require ((<=) 1) .gold) (addGold -1) (Just Furnish)
        ]


tileSpedizione : Tile
tileSpedizione =
    Tile "Spedizione"
        Rock
        9
        "assets/img/rounds/spedizione.jpg"
        priceFree
        noWalls
        -- TODO: all these actions are mutually exclusive.
        [ firstAction (require ((<=) 5) .wood) (\r -> r |> addWood -5 |> addGold 5) Nothing
        , secondAction (require ((<=) 5) .stone) (\r -> r |> addStone -5 |> addGold 5) Nothing
        , rightAction alwaysDoable (\r -> r) (Just Activate3)
        ]


tilePerforare : Tile
tilePerforare =
    Tile "Perforare"
        Rock
        8
        "assets/img/rounds/perforare.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (\r -> r) (Just Activate1)
        , bottomAction alwaysDoable (\r -> r) (Just Escavate1)
        ]



-- TODO: available only if player has more gold than opponent.


tileRinnovare : Tile
tileRinnovare =
    Tile "Rinnovare"
        Rock
        11
        "assets/img/rounds/rinnovare.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (\r -> r) (Just BuildWall)
        , bottomAction alwaysDoable (\r -> r) (Just Furnish)
        ]



-------------------------------------------
-------------EQUIPMENTS--------------------
-------------------------------------------


tileEmpty : Tile
tileEmpty =
    Tile "Empty Tile"
        Empty
        0
        ""
        priceFree
        (Walls None None None Placed)
        []



-- TODO: Handle Equipment Events


tileSotterraneo : Tile
tileSotterraneo =
    Tile "Sotterraneo"
        Rock
        11
        "assets/img/sotterraneo.jpg"
        (priceFree |> priceGold 4 |> priceStone 3)
        (Walls Placed Placed Placed Placed)
        []


tileLavorareIlLino : Tile
tileLavorareIlLino =
    Tile "Lavorare il Lino"
        Rock
        3
        "assets/img/lavorare_il_lino.jpg"
        (priceFree |> priceStone 1)
        (Walls Placed Optional Walls.None Placed)
        []


tileEquipaggiamenti : Tile
tileEquipaggiamenti =
    Tile "Equipaggiamenti"
        Rock
        3
        "assets/img/equipaggiamenti.jpg"
        (priceFree |> priceWood 2)
        (Walls Placed Walls.None Walls.None Optional)
        []


tileDepositoDiLegna : Tile
tileDepositoDiLegna =
    Tile "Deposito di Legna"
        Rock
        2
        "assets/img/deposito_di_legna.jpg"
        (priceFree |> priceStone 1)
        (Walls Placed Walls.None Walls.None Placed)
        []


tileAnalisiTerritoriale : Tile
tileAnalisiTerritoriale =
    Tile "Analisi Territoriale"
        Rock
        5
        "assets/img/deposito_di_legna.jpg"
        priceFree
        (Walls Placed Walls.None Walls.None Optional)
        []



-------------------------------------------
-------------ROOM TILES--------------------
-------------------------------------------


tileCaveEntrance : Tile
tileCaveEntrance =
    Tile "Entrata della Cava"
        Available
        0
        "assets/img/entrata_della_cava.jpg"
        priceFree
        noWalls
        [ firstAction alwaysDoable (addWood 1) Nothing
        , secondAction alwaysDoable (addStone 1) Nothing
        , thirdAction alwaysDoable (addEmmer 1) Nothing
        , fourthAction alwaysDoable (addFlax 1) Nothing
        ]


tileWarehouse : Tile
tileWarehouse =
    Tile "Magazzino"
        Rock
        2
        "assets/img/magazzino.jpg"
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
        ]


tileShelf : Tile
tileShelf =
    Tile "Shelf"
        Available
        3
        "assets/img/scaffale.jpg"
        (priceFree |> priceWood 1)
        (Walls Placed Walls.None Walls.None Walls.None)
        [ firstAction (require ((>) 2) .wood) (topWood 2) Nothing
        , secondAction (require ((>) 2) .stone) (topStone 2) Nothing
        , thirdAction (require ((>) 2) .emmer) (topEmmer 2) Nothing
        , fourthAction (require ((>) 2) .flax) (topFlax 2) Nothing
        ]


tileFoodCorner : Tile
tileFoodCorner =
    Tile "Angolo del Cibo"
        Available
        3
        "assets/img/angolo_del_cibo.jpg"
        (priceFree |> priceStone 1)
        (Walls Placed Walls.None Walls.None Placed)
        [ fullAction (require ((>) 3) .food) (topFood 3) Nothing ]


tileSpinningWheel : Tile
tileSpinningWheel =
    Tile "Filatoio"
        Available
        4
        "assets/img/filatoio.jpg"
        (priceFree |> priceWood 1)
        (Walls Placed Walls.None Walls.None Walls.None)
        [ leftAction (require ((<=) 1) .flax) (\res -> res |> addFlax -1 |> addGold 1) Nothing
        , rightAction (require ((<=) 3) .flax) (\res -> res |> addFlax -3 |> addGold 2) Nothing
        ]



-- TODO: these two actions are not mutually exclusive


tileTunnel : Tile
tileTunnel =
    Tile "Tunnel"
        Available
        3
        "assets/img/tunnel.jpg"
        (priceFree |> priceWood 1)
        (Walls Walls.None Placed Walls.None Placed)
        [ topAction alwaysDoable (addFood 2) Nothing
        , bottomAction (require ((>) 3) .stone) (\resources -> resources |> addStone 1 |> minStone 3) Nothing
        ]


tileAltareSacrificale : Tile
tileAltareSacrificale =
    Tile "Altare Sacrificale"
        Rock
        7
        "assets/img/altare_sacrificale.jpg"
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
        ]


tileBancarella : Tile
tileBancarella =
    Tile "Bancarella"
        Rock
        6
        "assets/img/bancarella.jpg"
        (priceFree |> priceWood 1 |> priceGold 1)
        (Walls Placed Optional Walls.None Optional)
        [ leftAction (require ((<=) 5) .emmer) (\res -> res |> addEmmer -5 |> addGold 4) Nothing
        , rightAction (require ((<=) 5) .flax) (\res -> res |> addFlax -5 |> addGold 4) Nothing
        ]


tileCameraSegreta : Tile
tileCameraSegreta =
    Tile "Camera Segreta"
        Rock
        8
        "assets/img/camera_segreta.jpg"
        (priceFree |> priceWood 2 |> priceStone 1)
        (Walls Placed Placed Placed Placed)
        [ leftAction alwaysDoable (\res -> res |> addFlax 3) Nothing
        , rightAction alwaysDoable (\res -> res |> addGold 1) Nothing
        ]


tileCavaInEspansione : Tile
tileCavaInEspansione =
    Tile "Cava in Espansione"
        Rock
        8
        "assets/img/cava_in_espansione.jpg"
        (priceFree |> priceWood 1 |> priceStone 3)
        (Walls Placed Placed Walls.None Placed)
        [ fullAction (require ((<=) 1) .gold) (\res -> res) Nothing ]


tileDeposito : Tile
tileDeposito =
    Tile "Deposito"
        Rock
        6
        "assets/img/deposito.jpg"
        (priceFree |> priceWood 2 |> priceGold 1)
        (Walls Placed Walls.None Walls.None Placed)
        [ fullAction alwaysDoable (\res -> res |> addEmmer 1 |> addFlax 1 |> addFood 1) Nothing ]


tileFiliera : Tile
tileFiliera =
    Tile "Filiera"
        Rock
        5
        "assets/img/filiera.jpg"
        (priceFree |> priceWood 2)
        (Walls Placed Walls.None Walls.None Placed)
        [ fullAction (require ((<=) 2) .flax) (\res -> res |> addFlax -2 |> addGold 2 |> addFood 2) Nothing ]


tileForno : Tile
tileForno =
    Tile "Forno"
        Rock
        6
        "assets/img/forno.jpg"
        (priceFree |> priceWood 1 |> priceStone 2)
        (Walls Placed Placed Walls.None Placed)
        [ leftAction (require ((<=) 2) .emmer) (\res -> res |> addEmmer -2 |> addFood 4 |> addGold 1) Nothing
        , rightAction (require ((<=) 3) .emmer) (\res -> res |> addEmmer -3 |> addFood 4 |> addGold 2) Nothing
        ]


tileMacina : Tile
tileMacina =
    Tile "Macina"
        Available
        4
        "assets/img/macina.jpg"
        (priceFree |> priceStone 1)
        (Walls Placed Walls.None Walls.None Optional)
        [ leftAction (require ((<=) 2) .emmer) (\res -> res |> addEmmer -1 |> addFood 3) Nothing
        , rightAction (require ((<=) 3) .emmer) (\res -> res |> addEmmer -4 |> addFood 7) Nothing
        ]


tileGoldMine : Tile
tileGoldMine =
    Tile "Miniera d'Oro"
        Rock
        9
        "assets/img/miniera_d_oro.jpg"
        (priceFree |> priceGold 5)
        (Walls Placed Optional Walls.None Placed)
        [ fullAction alwaysDoable (\res -> res |> addGold 1 |> addStone 1) Nothing ]


tileOfficina : Tile
tileOfficina =
    Tile "Officina"
        Rock
        5
        "assets/img/officina.jpg"
        (priceFree |> priceWood 1 |> priceStone 2)
        (Walls Placed Optional Walls.None Placed)
        [ fullAction
            (\res ->
                require ((<=) 1) .flax res
                    && require ((<=) 2) .food res
            )
            (\res -> res |> addWood -2 |> addFlax -1 |> addGold 3)
            Nothing
        ]


tileSalotto : Tile
tileSalotto =
    Tile "Salotto"
        Available
        6
        "assets/img/salotto.jpg"
        (priceFree |> priceStone 1 |> priceGold 1)
        (Walls Placed Placed Walls.None Placed)
        [ fullAction alwaysDoable (\res -> res |> topWood 1 |> topWood 1 |> topEmmer 1 |> topFlax 1 |> topFood 1 |> topGold 1) Nothing ]


tileLuxuryRoom : Tile
tileLuxuryRoom =
    Tile "Stanza di Lusso"
        Rock
        12
        "assets/img/stanza_di_lusso.jpg"
        (priceFree |> priceGold 7)
        (Walls Placed Placed Optional Placed)
        [ fullAction alwaysDoable (\res -> res |> addGold 1 |> addFlax 1) Nothing ]



-- TODO: the user should choose three resources to spend


tileStanzaDiSnodo : Tile
tileStanzaDiSnodo =
    Tile "Stanza di Snodo"
        Rock
        6
        "assets/img/stanza_di_snodo.jpg"
        (priceFree |> priceWood 2)
        (Walls Placed Placed Optional Placed)
        [ fullAction alwaysDoable (\res -> res |> addGold 2) Nothing ]


tileTesoreria : Tile
tileTesoreria =
    Tile "Tesoreria"
        Rock
        10
        "assets/img/tesoreria.jpg"
        (priceFree |> priceGold 3)
        (Walls Placed Placed Placed Placed)
        [ fullAction (require ((<=) 3) .gold) (\res -> res |> addGold -3 |> addGold 4 |> addFood 1) Nothing ]


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


firstAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> Action
firstAction isDoable do events =
    Action "first" True isDoable do events


secondAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> Action
secondAction isDoable do events =
    Action "second" True isDoable do events


thirdAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> Action
thirdAction isDoable do events =
    Action "third" True isDoable do events


fourthAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> Action
fourthAction isDoable do events =
    Action "fourth" True isDoable do events


leftAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> Action
leftAction isDoable do events =
    Action "left" True isDoable do events


rightAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> Action
rightAction isDoable do events =
    Action "right" True isDoable do events


topAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> Action
topAction isDoable do events =
    Action "top" True isDoable do events


topLeftAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> Action
topLeftAction isDoable do events =
    Action "topleft" True isDoable do events


topRightAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> Action
topRightAction isDoable do events =
    Action "topright" True isDoable do events


bottomLeftAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> Action
bottomLeftAction isDoable do events =
    Action "bottomleft" True isDoable do events


bottomRightAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> Action
bottomRightAction isDoable do events =
    Action "bottomright" True isDoable do events


bottomAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> Action
bottomAction isDoable do events =
    Action "bottom" True isDoable do events


fullAction : (Resources -> Bool) -> (Resources -> Resources) -> Maybe Subphase -> Action
fullAction isDoable do events =
    Action "full" True isDoable do events
