module Tiles exposing (..)

import Resources exposing (Resources, noWalls, priceFree, priceGold, priceStone, priceWood)
import Walls exposing (Wall(..), Walls)


type alias RoomTile state =
    { title : String
    , score : Int
    , src : String
    , price : Resources
    , walls : Walls
    , actions : Actions state
    }


type alias ActionTile state =
    { hidden : Bool
    , actions : List (Action state)
    }


type Actions state
    = Actions (List (Action state))


type alias Action state =
    { classes: String
    , isDoable : state -> Bool
    , do : state -> state
    }


firstAction: (state -> Bool) -> (state -> state) -> Action state
firstAction isDoable do =
    Action "first" isDoable do


secondAction: (state -> Bool) -> (state -> state) -> Action state
secondAction isDoable do =
    Action "second" isDoable do


thirdAction: (state -> Bool) -> (state -> state) -> Action state
thirdAction isDoable do =
    Action "third" isDoable do


fourthAction: (state -> Bool) -> (state -> state) -> Action state
fourthAction isDoable do =
    Action "fourth" isDoable do


leftAction: (state -> Bool) -> (state -> state) -> Action state
leftAction isDoable do =
    Action "left" isDoable do


rightAction: (state -> Bool) -> (state -> state) -> Action state
rightAction isDoable do =
    Action "right" isDoable do


topAction: (state -> Bool) -> (state -> state) -> Action state
topAction isDoable do =
    Action "top" isDoable do


bottomAction: (state -> Bool) -> (state -> state) -> Action state
bottomAction isDoable do =
    Action "bottom" isDoable do


fullAction: (state -> Bool) -> (state -> state) -> Action state
fullAction isDoable do =
    Action "full" isDoable do


-------------------------------------------
------------------TILES--------------------
-------------------------------------------

tileCaveEntrance : RoomTile Resources
tileCaveEntrance =
    RoomTile "Entrata della Cava"
        0
        "assets/img/entrata_della_cava.jpg"
        priceFree
        noWalls
        (Actions
            [ firstAction alwaysDoable (addWood 1)
            , secondAction alwaysDoable (addStone 1)
            , thirdAction alwaysDoable (addEmmer 1)
            , fourthAction alwaysDoable (addFlax 1)
            ]
        )


tileWarehouse : RoomTile Resources
tileWarehouse =
    RoomTile "Magazzino"
        2
        "assets/img/magazzino.jpg"
        (priceFree |> priceWood 2)
        (Walls Required Optional None Optional)
        (Actions
            [ fullAction (require ((<) 2) .food) (\board -> board
                                                            |> addFood -2
                                                            |> addWood 1
                                                            |> addStone 1
                                                            |> addFlax 1
                                                            |> addEmmer 1)])


tileShelf : RoomTile Resources
tileShelf =
    RoomTile "Shelf"
        3
        "assets/img/scaffale.jpg"
        (priceFree |> priceWood 1)
        (Walls Required None None None)
        (Actions
            [ firstAction (require ((>) 2) .wood ) (topWood 2)
            , secondAction (require ((>) 2) .stone) (topStone 2)
            , thirdAction (require ((>) 2) .emmer) (topEmmer 2)
            , fourthAction (require ((>) 2) .flax ) (topFlax 2)
            ]
        )


tileFoodCorner: RoomTile Resources
tileFoodCorner =
    RoomTile "Angolo del Cibo"
        3
        "assets/img/angolo_del_cibo.jpg"
        (priceFree |> priceStone 1)
        (Walls Required None None Required)
        (Actions
            [ fullAction (require ((>) 3) .food) (topFood 3)]
        )


tileSpinningWheel: RoomTile Resources
tileSpinningWheel =
    RoomTile "Filatoio"
        4
        "assets/img/filatoio.jpg"
        (priceFree |> priceWood 1)
        (Walls Required None None None)
        (Actions
            [ leftAction (require ((<=) 1) .flax) (\board -> board |> addFlax -1 |> addGold 1)
            , rightAction (require ((<=) 3) .flax) (\board -> board |> addFlax -3 |> addGold 2)
            ]
        )


-- TODO: these two actions are not mutually exclusive
tileTunnel: RoomTile Resources
tileTunnel =
    RoomTile "Tunnel"
        3
        "assets/img/tunnel.jpg"
        (priceFree |> priceWood 1)
        (Walls None Required None Required)
        (Actions
            [ topAction alwaysDoable (addFood 2)
            , bottomAction (require ((>) 3) .stone) (\resources -> resources |> addStone 1 |> minStone 3)
            ]
        )


tileAltareSacrificale: RoomTile Resources
tileAltareSacrificale =
    RoomTile "Altare Sacrificale"
        7
        "assets/img/altare_sacrificale.jpg"
        (priceFree |> priceStone 4)
        (Walls Required Optional None Optional)
        (Actions
            [ fullAction (\res ->
                (require ((<=) 1) .wood res) &&
                (require ((<=) 1) .emmer res) &&
                (require ((<=) 1) .flax res) &&
                (require ((<=) 1) .food res)) (\res -> res |> addEmmer -1 |> addWood -1 |> addFlax -1 |> addFood -1 |> addGold 3)]
        )

tileBancarella: RoomTile Resources
tileBancarella =
    RoomTile "Bancarella"
        6
        "assets/img/bancarella.jpg"
        (priceFree |> priceWood 1 |> priceGold 1)
        (Walls Required Optional None Optional)
        (Actions
            [ leftAction (require ((<=) 5) .emmer) (\res -> res |> addEmmer -5 |> addGold 4)
            , rightAction (require ((<=) 5) .flax) (\res -> res |> addFlax -5 |> addGold 4)
            ]
        )

tileCameraSegreta: RoomTile Resources
tileCameraSegreta =
    RoomTile "Camera Segreta"
        8
        "assets/img/camera_segreta.jpg"
        (priceFree |> priceWood 2 |> priceStone 1)
        (Walls Required Required Required Required)
        (Actions
            [ leftAction alwaysDoable (\res -> res |> addEmmer 3 )
            , rightAction alwaysDoable (\res -> res |> addGold 1)
            ]
        )

-- TODO: Ask to Player to choose which rock to remove
tileCavaInEspansione: RoomTile Resources
tileCavaInEspansione =
    RoomTile "Cava in Espansione"
        8
        "assets/img/cava_in_espansione.jpg"
        (priceFree |> priceWood 1 |> priceStone 3)
        (Walls Required Required None Required)
        (Actions
            [ fullAction (require ((<=) 1) .gold) (\res -> res)
            ]
        )


tileDeposito: RoomTile Resources
tileDeposito =
    RoomTile "Deposito"
        6
        "assets/img/deposito.jpg"
        (priceFree |> priceWood 2 |> priceGold 1)
        (Walls Required None None Required)
        (Actions
            [ fullAction alwaysDoable (\res -> res |> addEmmer 1 |> addFlax 1 |> addFood 1 )]
        )


tileFiliera: RoomTile Resources
tileFiliera =
    RoomTile "Filiera"
        5
        "assets/img/filiera.jpg"
        (priceFree |> priceWood 2)
        (Walls Required None None Required)
        (Actions
            [fullAction (require ((<=) 2) .flax) (\res -> res |> addFlax -2 |> addGold 2 |> addFood 2 )]
        )


tileForno: RoomTile Resources
tileForno =
    RoomTile "Forno"
        6
        "assets/img/forno.jpg"
        (priceFree |> priceWood 1 |> priceStone 2)
        (Walls Required Required None Required)
        (Actions
            [ leftAction (require ((<=) 2) .emmer) (\res -> res |> addEmmer -2 |> addFood 4 |> addGold 1)
            , rightAction (require ((<=) 3) .emmer) (\res -> res |> addEmmer -3 |> addFood 4 |> addGold 2)]
        )


tileMacina: RoomTile Resources
tileMacina =
    RoomTile "Macina"
        4
        "assets/img/macina.jpg"
        (priceFree |> priceStone 1)
        (Walls Required None None Optional)
        (Actions
            [ leftAction (require ((<=) 2) .emmer) (\res -> res |> addEmmer -1 |> addFood 3 )
            , rightAction (require ((<=) 3) .emmer) (\res -> res |> addEmmer -4 |> addFood 7)]
        )


tileGoldMine: RoomTile Resources
tileGoldMine =
    RoomTile "Miniera d'Oro"
        9
        "assets/img/miniera_d_oro.jpg"
        (priceFree |> priceGold 5)
        (Walls Required Optional None Required)
        (Actions
            [ fullAction alwaysDoable (\res -> res |> addGold 1 |> addStone 1)])


tileOfficina: RoomTile Resources
tileOfficina =
    RoomTile "Officina"
        5
        "assets/img/officina.jpg"
        (priceFree |> priceWood 1 |> priceStone 2)
        (Walls Required Optional None Required)
        (Actions
            [ fullAction (\res ->
                (require ((<=) 1) .flax res) &&
                (require ((<=) 2) .food res))
                (\res -> res |> addWood -2 |> addFlax -1 |> addGold 3)])


tileSalotto: RoomTile Resources
tileSalotto =
    RoomTile "Salotto"
        6
        "assets/img/salotto.jpg"
        (priceFree |> priceStone 1 |> priceGold 1)
        (Walls Required Required None Required)
        (Actions
            [ fullAction alwaysDoable (\res -> res |> topWood 1 |> topWood 1 |> topEmmer 1 |> topFlax 1 |> topFood 1 |> topGold 1 )])


tileLuxuryRoom: RoomTile Resources
tileLuxuryRoom =
    RoomTile "Stanza di Lusso"
        12
        "assets/img/stanza_di_lusso.jpg"
        (priceFree |> priceGold 7)
        (Walls Required Required Optional Required)
        (Actions
            [ fullAction alwaysDoable (\res -> res |> addGold 1 |> addFlax 1)])


-- TODO: the user should choose three resources to spend
tileStanzaDiSnodo: RoomTile Resources
tileStanzaDiSnodo =
    RoomTile "Stanza di Snodo"
        6
        "assets/img/stanza_di_snodo.jpg"
        (priceFree |> priceWood 2)
        (Walls Required Required Optional Required)
        (Actions
            [ fullAction alwaysDoable (\res -> res |> addGold 2)])


tileTesoreria: RoomTile Resources
tileTesoreria =
    RoomTile "Tesoreria"
        10
        "assets/img/tesoreria.jpg"
        (priceFree |> priceGold 3)
        (Walls Required Required Required Required)
        (Actions
            [ fullAction (require ((<=) 3) .gold) (\res -> res |> addGold -3 |> addGold 4 |> addFood 1)])


require: (Int -> Bool) -> (Resources -> Int) -> Resources -> Bool
require condition getter resources =
    condition (getter resources)


topWood : Int -> Resources -> Resources
topWood qty resources  =
    { resources | wood = Basics.max resources.wood qty }


topFood : Int -> Resources -> Resources
topFood qty resources  =
    { resources | food = Basics.max resources.food qty }


topStone : Int -> Resources -> Resources
topStone qty resources  =
    { resources | stone = Basics.max resources.stone qty }


topGold : Int -> Resources -> Resources
topGold qty resources  =
    { resources | gold = Basics.max resources.gold qty }


minStone : Int -> Resources -> Resources
minStone qty resources  =
    { resources | stone = Basics.min resources.stone qty }


topFlax : Int -> Resources -> Resources
topFlax qty resources  =
    { resources | flax = Basics.max resources.flax qty }


topEmmer : Int -> Resources -> Resources
topEmmer qty resources  =
    { resources | emmer = Basics.max resources.emmer qty }


addWood : Int -> Resources -> Resources
addWood qty resources  =
    { resources | wood = min 9 (resources.wood + qty) }


addStone : Int -> Resources -> Resources
addStone qty resources  =
    { resources | stone = min 9 (resources.stone + qty) }


addFlax : Int -> Resources -> Resources
addFlax qty resources  =
    { resources | flax = min 9 (resources.flax + qty) }


addEmmer : Int -> Resources -> Resources
addEmmer qty resources  =
    { resources | emmer = min 9 (resources.emmer + qty) }


addFood : Int -> Resources -> Resources
addFood qty resources  =
    { resources | food = min 9 (resources.food + qty) }


addGold : Int -> Resources -> Resources
addGold qty resources  =
    { resources | gold = min 19 (resources.gold + qty) }


alwaysDoable : Resources -> Bool
alwaysDoable board =
    True