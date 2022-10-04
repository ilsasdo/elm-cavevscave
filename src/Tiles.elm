module Tiles exposing (..)

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Resources exposing (Resources, noWalls, priceFood, priceFree, priceGold, priceStone, priceWood)
import Walls exposing (Wall(..), Walls)


type TileStatus
    = Available
    | Active
    | Rock
    | Empty


type alias RoomTile state msg =
    { title : String
    , status : TileStatus
    , score : Int
    , src : String
    , price : Resources
    , walls : Walls
    , actions : List (Action state msg)
    }


type alias Action state msg =
    { classes : String
    , available : Bool
    , isDoable : state -> Bool
    , do : state -> state
    , actionClick : Event msg
    }


type Event msg
    = OnClick (RoomTile Resources msg -> Action Resources msg -> msg)


tileSetStatus tile status tiles =
    List.map
        (\t ->
            if t.title == tile.title then
                { t | status = status }

            else
                t
        )
        tiles


viewTile : List (Attribute msg) -> Resources -> RoomTile Resources msg -> Html msg
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


viewAction : RoomTile Resources msg -> Resources -> Action Resources msg -> Int -> Html msg
viewAction tile resources action index =
    let
        newTile =
            { tile | actions = consumeAction tile.actions index }
    in
    if action.available && action.isDoable resources then
        div [ class ("action doable " ++ action.classes), onClick (trigger newTile action) ] []

    else
        div [ class ("action notdoable " ++ action.classes) ] []


trigger : RoomTile Resources msg -> Action Resources msg -> msg
trigger tile action =
    case action.actionClick of
        OnClick msg ->
            msg tile action


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


tileLavoriDomestici : Event msg -> RoomTile Resources msg
tileLavoriDomestici furnishCavern =
    RoomTile "Lavori Domestici"
        Available
        0
        "assets/img/rounds/lavori_domestici.jpg"
        priceFree
        noWalls
        [ topAction (\r -> r.food > r.actions) (\r -> r |> addFood r.actions) furnishCavern

        -- TODO: left and right action are mutually exclusives
        , bottomLeftAction (require ((<=) 5) .food) (addFood -5) furnishCavern
        , bottomRightAction (require ((<=) 1) .gold) (addGold -1) furnishCavern
        ]


tileColtivare : Event msg -> Event msg -> RoomTile Resources msg
tileColtivare actionClick activate1 =
    RoomTile "Coltivare"
        Available
        0
        "assets/img/rounds/coltivare.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (\r -> r) activate1
        , bottomAction alwaysDoable (\r -> r |> addEmmer 2 |> addFlax 1) actionClick
        ]


tileSottobosco : Event msg -> Event msg -> RoomTile Resources msg
tileSottobosco actionClick activate1 =
    RoomTile "Sottobosco"
        Available
        0
        "assets/img/rounds/sottobosco.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (\r -> r) activate1
        , bottomAction alwaysDoable (addWood 2) actionClick
        ]


tileScavare : Event msg -> Event msg -> Event msg -> RoomTile Resources msg
tileScavare actionClick escavate escavateTwice =
    RoomTile "Scavare"
        Available
        0
        "assets/img/rounds/scavare.jpg"
        priceFree
        noWalls
        [ topLeftAction alwaysDoable (\r -> r) escavate
        , topRightAction (require ((<=) 2) .food) (addFood -2) escavateTwice
        , bottomAction alwaysDoable (addStone 1) actionClick
        ]


tileArredare : Event msg -> Event msg -> RoomTile Resources msg
tileArredare actionClick furnishCavern =
    RoomTile "Arredare"
        Rock
        0
        "assets/img/rounds/arredare.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (addFood 1) actionClick
        , bottomAction (\r -> r.food > r.actions) (\r -> r |> addFood r.actions) furnishCavern
        ]


tileCostruireUnMuro : Event msg -> Event msg -> Event msg -> RoomTile Resources msg
tileCostruireUnMuro actionClick activate1 buildWall =
    RoomTile "Costrurire un Muro"
        Rock
        0
        "assets/img/rounds/costruire_un_muro.jpg"
        priceFree
        noWalls
        -- TODO: third and fourth action are mutually exclusives
        [ topLeftAction alwaysDoable (\r -> r) activate1
        , thirdAction alwaysDoable (addWood 1) actionClick
        , fourthAction alwaysDoable (addStone 1) actionClick
        , bottomAction alwaysDoable (\r -> r) buildWall -- TODO: available only if there are walls to build
        ]


tileMinare : Event msg -> Event msg -> RoomTile Resources msg
tileMinare activate2 escavateThroughWalls =
    RoomTile "Minare"
        Rock
        0
        "assets/img/rounds/minare.jpg"
        priceFree
        noWalls
        [ leftAction alwaysDoable (\r -> r) activate2
        , rightAction alwaysDoable (\r -> r) escavateThroughWalls
        ]



--TODO: available only if there are walls to destroy


tileDemolireUnMuro : Event msg -> RoomTile Resources msg
tileDemolireUnMuro destroyWall =
    RoomTile "Demolire un Muro"
        Rock
        0
        "assets/img/rounds/demolire_un_muro.jpg"
        priceFree
        noWalls
        -- TODO: resources should be update after wall choice
        [ fullAction alwaysDoable (\r -> r |> addStone 2 |> addFood 3 |> addGold 1) destroyWall ]


tileEspansione : Event msg -> Event msg -> RoomTile Resources msg
tileEspansione escavate furnish =
    RoomTile "Espansione"
        Rock
        0
        "assets/img/rounds/espansione.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (\r -> r) escavate

        -- TODO: left and right action are mutually exclusives
        , bottomLeftAction (require ((<=) 5) .food) (addFood -5) furnish
        , bottomRightAction (require ((<=) 1) .gold) (addGold -1) furnish
        ]


tileSpedizione : Event msg -> Event msg -> RoomTile Resources msg
tileSpedizione actionClick activate3 =
    RoomTile "Spedizione"
        Rock
        0
        "assets/img/rounds/spedizione.jpg"
        priceFree
        noWalls
        -- TODO: all these actions are mutually exclusive.
        [ firstAction (require ((<=) 5) .wood) (\r -> r |> addWood -5 |> addGold 5) actionClick
        , secondAction (require ((<=) 5) .stone) (\r -> r |> addStone -5 |> addGold 5) actionClick
        , rightAction alwaysDoable (\r -> r) activate3
        ]


tilePerforare : Event msg -> Event msg -> RoomTile Resources msg
tilePerforare activate1 escavate =
    RoomTile "Perforare"
        Rock
        0
        "assets/img/rounds/perforare.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (\r -> r) activate1
        , bottomAction alwaysDoable (\r -> r) escavate
        ]



-- TODO: available only if player has more gold than opponent.


tileRinnovare : Event msg -> Event msg -> RoomTile Resources msg
tileRinnovare buildWall furnish =
    RoomTile "Rinnovare"
        Rock
        0
        "assets/img/rounds/rinnovare.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (\r -> r) buildWall
        , bottomAction alwaysDoable (\r -> r) furnish
        ]



-------------------------------------------
-------------EQUIPMENTS--------------------
-------------------------------------------


tileEmpty : RoomTile Resources msg
tileEmpty =
    RoomTile "Empty Tile"
        Empty
        0
        ""
        priceFree
        (Walls None None None None)
        []



-- TODO: Handle Equipment Events


tileSotterraneo : Event msg -> RoomTile Resources msg
tileSotterraneo actionClick =
    RoomTile "Sotterraneo"
        Rock
        11
        "assets/img/sotterraneo.jpg"
        (priceFree |> priceGold 4 |> priceStone 3)
        (Walls Placed Placed Placed Placed)
        []


tileLavorareIlLino : Event msg -> RoomTile Resources msg
tileLavorareIlLino actionClick =
    RoomTile "Lavorare il Lino"
        Rock
        3
        "assets/img/lavorare_il_lino.jpg"
        (priceFree |> priceStone 1)
        (Walls Placed Optional Walls.None Placed)
        []


tileEquipaggiamenti : Event msg -> RoomTile Resources msg
tileEquipaggiamenti actionClick =
    RoomTile "Equipaggiamenti"
        Rock
        3
        "assets/img/equipaggiamenti.jpg"
        (priceFree |> priceWood 2)
        (Walls Placed Walls.None Walls.None Optional)
        []


tileDepositoDiLegna : Event msg -> RoomTile Resources msg
tileDepositoDiLegna actionClick =
    RoomTile "Deposito di Legna"
        Rock
        2
        "assets/img/deposito_di_legna.jpg"
        (priceFree |> priceStone 1)
        (Walls Placed Walls.None Walls.None Placed)
        []


tileAnalisiTerritoriale : Event msg -> RoomTile Resources msg
tileAnalisiTerritoriale actionClick =
    RoomTile "Analisi Territoriale"
        Rock
        5
        "assets/img/deposito_di_legna.jpg"
        priceFree
        (Walls Placed Walls.None Walls.None Optional)
        []



-------------------------------------------
-------------ROOM TILES--------------------
-------------------------------------------


tileCaveEntrance : Event msg -> RoomTile Resources msg
tileCaveEntrance actionClick =
    RoomTile "Entrata della Cava"
        Available
        0
        "assets/img/entrata_della_cava.jpg"
        priceFree
        noWalls
        [ firstAction alwaysDoable (addWood 1) actionClick
        , secondAction alwaysDoable (addStone 1) actionClick
        , thirdAction alwaysDoable (addEmmer 1) actionClick
        , fourthAction alwaysDoable (addFlax 1) actionClick
        ]


tileWarehouse : Event msg -> RoomTile Resources msg
tileWarehouse actionClick =
    RoomTile "Magazzino"
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
            actionClick
        ]


tileShelf : Event msg -> RoomTile Resources msg
tileShelf actionClick =
    RoomTile "Shelf"
        Available
        3
        "assets/img/scaffale.jpg"
        (priceFree |> priceWood 1)
        (Walls Placed Walls.None Walls.None Walls.None)
        [ firstAction (require ((>) 2) .wood) (topWood 2) actionClick
        , secondAction (require ((>) 2) .stone) (topStone 2) actionClick
        , thirdAction (require ((>) 2) .emmer) (topEmmer 2) actionClick
        , fourthAction (require ((>) 2) .flax) (topFlax 2) actionClick
        ]


tileFoodCorner : Event msg -> RoomTile Resources msg
tileFoodCorner actionClick =
    RoomTile "Angolo del Cibo"
        Available
        3
        "assets/img/angolo_del_cibo.jpg"
        (priceFree |> priceStone 1)
        (Walls Placed Walls.None Walls.None Placed)
        [ fullAction (require ((>) 3) .food) (topFood 3) actionClick ]


tileSpinningWheel : Event msg -> RoomTile Resources msg
tileSpinningWheel actionClick =
    RoomTile "Filatoio"
        Available
        4
        "assets/img/filatoio.jpg"
        (priceFree |> priceWood 1)
        (Walls Placed Walls.None Walls.None Walls.None)
        [ leftAction (require ((<=) 1) .flax) (\res -> res |> addFlax -1 |> addGold 1) actionClick
        , rightAction (require ((<=) 3) .flax) (\res -> res |> addFlax -3 |> addGold 2) actionClick
        ]



-- TODO: these two actions are not mutually exclusive


tileTunnel : Event msg -> RoomTile Resources msg
tileTunnel actionClick =
    RoomTile "Tunnel"
        Available
        3
        "assets/img/tunnel.jpg"
        (priceFree |> priceWood 1)
        (Walls Walls.None Placed Walls.None Placed)
        [ topAction alwaysDoable (addFood 2) actionClick
        , bottomAction (require ((>) 3) .stone) (\resources -> resources |> addStone 1 |> minStone 3) actionClick
        ]


tileAltareSacrificale : Event msg -> RoomTile Resources msg
tileAltareSacrificale actionClick =
    RoomTile "Altare Sacrificale"
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
            actionClick
        ]


tileBancarella : Event msg -> RoomTile Resources msg
tileBancarella actionClick =
    RoomTile "Bancarella"
        Rock
        6
        "assets/img/bancarella.jpg"
        (priceFree |> priceWood 1 |> priceGold 1)
        (Walls Placed Optional Walls.None Optional)
        [ leftAction (require ((<=) 5) .emmer) (\res -> res |> addEmmer -5 |> addGold 4) actionClick
        , rightAction (require ((<=) 5) .flax) (\res -> res |> addFlax -5 |> addGold 4) actionClick
        ]


tileCameraSegreta : Event msg -> RoomTile Resources msg
tileCameraSegreta actionClick =
    RoomTile "Camera Segreta"
        Rock
        8
        "assets/img/camera_segreta.jpg"
        (priceFree |> priceWood 2 |> priceStone 1)
        (Walls Placed Placed Placed Placed)
        [ leftAction alwaysDoable (\res -> res |> addFlax 3) actionClick
        , rightAction alwaysDoable (\res -> res |> addGold 1) actionClick
        ]


tileCavaInEspansione : Event msg -> RoomTile Resources msg
tileCavaInEspansione actionClick =
    RoomTile "Cava in Espansione"
        Rock
        8
        "assets/img/cava_in_espansione.jpg"
        (priceFree |> priceWood 1 |> priceStone 3)
        (Walls Placed Placed Walls.None Placed)
        [ fullAction (require ((<=) 1) .gold) (\res -> res) actionClick ]


tileDeposito : Event msg -> RoomTile Resources msg
tileDeposito actionClick =
    RoomTile "Deposito"
        Rock
        6
        "assets/img/deposito.jpg"
        (priceFree |> priceWood 2 |> priceGold 1)
        (Walls Placed Walls.None Walls.None Placed)
        [ fullAction alwaysDoable (\res -> res |> addEmmer 1 |> addFlax 1 |> addFood 1) actionClick ]


tileFiliera : Event msg -> RoomTile Resources msg
tileFiliera actionClick =
    RoomTile "Filiera"
        Rock
        5
        "assets/img/filiera.jpg"
        (priceFree |> priceWood 2)
        (Walls Placed Walls.None Walls.None Placed)
        [ fullAction (require ((<=) 2) .flax) (\res -> res |> addFlax -2 |> addGold 2 |> addFood 2) actionClick ]


tileForno : Event msg -> RoomTile Resources msg
tileForno actionClick =
    RoomTile "Forno"
        Rock
        6
        "assets/img/forno.jpg"
        (priceFree |> priceWood 1 |> priceStone 2)
        (Walls Placed Placed Walls.None Placed)
        [ leftAction (require ((<=) 2) .emmer) (\res -> res |> addEmmer -2 |> addFood 4 |> addGold 1) actionClick
        , rightAction (require ((<=) 3) .emmer) (\res -> res |> addEmmer -3 |> addFood 4 |> addGold 2) actionClick
        ]


tileMacina : Event msg -> RoomTile Resources msg
tileMacina actionClick =
    RoomTile "Macina"
        Available
        4
        "assets/img/macina.jpg"
        (priceFree |> priceStone 1)
        (Walls Placed Walls.None Walls.None Optional)
        [ leftAction (require ((<=) 2) .emmer) (\res -> res |> addEmmer -1 |> addFood 3) actionClick
        , rightAction (require ((<=) 3) .emmer) (\res -> res |> addEmmer -4 |> addFood 7) actionClick
        ]


tileGoldMine : Event msg -> RoomTile Resources msg
tileGoldMine actionClick =
    RoomTile "Miniera d'Oro"
        Rock
        9
        "assets/img/miniera_d_oro.jpg"
        (priceFree |> priceGold 5)
        (Walls Placed Optional Walls.None Placed)
        [ fullAction alwaysDoable (\res -> res |> addGold 1 |> addStone 1) actionClick ]


tileOfficina : Event msg -> RoomTile Resources msg
tileOfficina actionClick =
    RoomTile "Officina"
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
            actionClick
        ]


tileSalotto : Event msg -> RoomTile Resources msg
tileSalotto actionClick =
    RoomTile "Salotto"
        Available
        6
        "assets/img/salotto.jpg"
        (priceFree |> priceStone 1 |> priceGold 1)
        (Walls Placed Placed Walls.None Placed)
        [ fullAction alwaysDoable (\res -> res |> topWood 1 |> topWood 1 |> topEmmer 1 |> topFlax 1 |> topFood 1 |> topGold 1) actionClick ]


tileLuxuryRoom : Event msg -> RoomTile Resources msg
tileLuxuryRoom actionClick =
    RoomTile "Stanza di Lusso"
        Rock
        12
        "assets/img/stanza_di_lusso.jpg"
        (priceFree |> priceGold 7)
        (Walls Placed Placed Optional Placed)
        [ fullAction alwaysDoable (\res -> res |> addGold 1 |> addFlax 1) actionClick ]



-- TODO: the user should choose three resources to spend


tileStanzaDiSnodo : Event msg -> RoomTile Resources msg
tileStanzaDiSnodo actionClick =
    RoomTile "Stanza di Snodo"
        Rock
        6
        "assets/img/stanza_di_snodo.jpg"
        (priceFree |> priceWood 2)
        (Walls Placed Placed Optional Placed)
        [ fullAction alwaysDoable (\res -> res |> addGold 2) actionClick ]


tileTesoreria : Event msg -> RoomTile Resources msg
tileTesoreria actionClick =
    RoomTile "Tesoreria"
        Rock
        10
        "assets/img/tesoreria.jpg"
        (priceFree |> priceGold 3)
        (Walls Placed Placed Placed Placed)
        [ fullAction (require ((<=) 3) .gold) (\res -> res |> addGold -3 |> addGold 4 |> addFood 1) actionClick ]


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


firstAction : (state -> Bool) -> (state -> state) -> Event msg -> Action state msg
firstAction isDoable do events =
    Action "first" True isDoable do events


secondAction : (state -> Bool) -> (state -> state) -> Event msg -> Action state msg
secondAction isDoable do events =
    Action "second" True isDoable do events


thirdAction : (state -> Bool) -> (state -> state) -> Event msg -> Action state msg
thirdAction isDoable do events =
    Action "third" True isDoable do events


fourthAction : (state -> Bool) -> (state -> state) -> Event msg -> Action state msg
fourthAction isDoable do events =
    Action "fourth" True isDoable do events


leftAction : (state -> Bool) -> (state -> state) -> Event msg -> Action state msg
leftAction isDoable do events =
    Action "left" True isDoable do events


rightAction : (state -> Bool) -> (state -> state) -> Event msg -> Action state msg
rightAction isDoable do events =
    Action "right" True isDoable do events


topAction : (state -> Bool) -> (state -> state) -> Event msg -> Action state msg
topAction isDoable do events =
    Action "top" True isDoable do events


topLeftAction : (state -> Bool) -> (state -> state) -> Event msg -> Action state msg
topLeftAction isDoable do events =
    Action "topleft" True isDoable do events


topRightAction : (state -> Bool) -> (state -> state) -> Event msg -> Action state msg
topRightAction isDoable do events =
    Action "topright" True isDoable do events


bottomLeftAction : (state -> Bool) -> (state -> state) -> Event msg -> Action state msg
bottomLeftAction isDoable do events =
    Action "bottomleft" True isDoable do events


bottomRightAction : (state -> Bool) -> (state -> state) -> Event msg -> Action state msg
bottomRightAction isDoable do events =
    Action "bottomright" True isDoable do events


bottomAction : (state -> Bool) -> (state -> state) -> Event msg -> Action state msg
bottomAction isDoable do events =
    Action "bottom" True isDoable do events


fullAction : (state -> Bool) -> (state -> state) -> Event msg -> Action state msg
fullAction isDoable do events =
    Action "full" True isDoable do events
