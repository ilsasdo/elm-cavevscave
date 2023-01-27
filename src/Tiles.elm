module Tiles exposing (..)

import Array exposing (Array)
import Game exposing (Action, GameMsg(..), Resources, Subphase(..), Tile, TileStatus(..), TileType(..), Wall(..))
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Random
import Random.List
import Resources exposing (addEmmer, addFlax, addFood, addGold, addStone, addWood, alwaysDoable, minStone, priceFood, priceFree, priceGold, priceStone, priceWood, require, topEmmer, topFlax, topFood, topGold, topStone, topWood)
import Walls exposing (noWalls)


initRandomTiles =
    setupRandomTiles
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
        , tileProspectingSite
        , tileDungeon
        , tileEquipmentRoom
        , tileRettingRoom
        , tileWoodStoreroom
        ]
        [ tileLavoriDomestici
        , tileColtivare
        , tileSottobosco
        , tileScavare
        ]
        [ tileArredare
        , tileCostruireUnMuro
        , tileMinare
        ]
        [ tileDemolireUnMuro
        , tileEspansione
        , tileSpedizione
        , tilePerforare
        ]
        [ tileRinnovare ]


initCommonRooms : List Tile
initCommonRooms =
    [ tileShelf
    , tileSpinningWheel
    , tileMacina
    , tileSalotto
    , tileTunnel
    , tileFoodCorner
    ]


setupRandomTiles : List Tile -> List Tile -> List Tile -> List Tile -> List Tile -> Cmd GameMsg
setupRandomTiles rooms round1Tiles round2Tiles round3Tiles round4Tiles =
    Cmd.batch
        [ Random.generate InitPlayerBoard (Random.List.shuffle rooms)
        , Random.generate InitRoundTiles (Random.List.shuffle round4Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round3Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round2Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round1Tiles)
        ]


updateStatus tile status tiles =
    List.map
        (\t ->
            if t.title == tile.title then
                { t | status = status }

            else
                t
        )
        tiles

deactivateTiles tiles =
    List.map
        (\t ->
            if t.status == Game.Active then
                { t | status = Game.Available }

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
                { tile | walls = Game.Walls Game.Placed (Walls.get 0 walls) (Walls.get 1 walls) Game.Placed }

            1 ->
                { tile | walls = Game.Walls Game.Placed Game.Placed (Walls.get 2 walls) (Walls.get 0 walls) }

            2 ->
                { tile | walls = Game.Walls (Walls.get 1 walls) (Walls.get 3 walls) (Walls.get 4 walls) Game.Placed }

            3 ->
                { tile | walls = Game.Walls (Walls.get 2 walls) Game.Placed (Walls.get 5 walls) (Walls.get 3 walls) }

            4 ->
                { tile | walls = Game.Walls (Walls.get 4 walls) (Walls.get 6 walls) (Walls.get 7 walls) Game.Placed }

            5 ->
                { tile | walls = Game.Walls (Walls.get 5 walls) Game.Placed (Walls.get 8 walls) (Walls.get 6 walls) }

            6 ->
                { tile | walls = Game.Walls (Walls.get 7 walls) (Walls.get 9 walls) (Walls.get 10 walls) Game.Placed }

            7 ->
                { tile | walls = Game.Walls (Walls.get 8 walls) Game.Placed (Walls.get 9 walls) (Walls.get 11 walls) }

            8 ->
                { tile | walls = Game.Walls (Walls.get 10 walls) (Walls.get 12 walls) Game.Placed Game.Placed }

            9 ->
                { tile | walls = Game.Walls (Walls.get 11 walls) (Walls.get 13 walls) Game.Placed (Walls.get 12 walls) }

            10 ->
                { tile | walls = Game.Walls Game.Placed Game.Placed Game.Placed (Walls.get 13 walls) }

            _ ->
                tile


viewTile : List (Attribute GameMsg) -> Resources -> Tile -> Html GameMsg
viewTile attributes resources tile =
    div attributes
        [ case tile.status of
            Active ->
                div
                    [ style "background-image" ("url(" ++ tile.src ++ ")")
                    , class "tile"
                    ]
                    (List.map (viewAction tile resources) tile.actions)

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


viewAction : Tile -> Resources -> Action -> Html GameMsg
viewAction tile resources action =
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


setStatus : TileStatus -> Tile -> Tile
setStatus status tile =
    { tile | status = status }



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
        [ firstAction (require .emmer (>) 0) (\r -> r |> addFood 1 |> addEmmer -1) Nothing []
        , secondAction (require .flax (>) 0) (\r -> r |> addFood 1 |> addFlax -1) Nothing []
        , thirdAction (require .gold (>) 0) (\r -> r |> addFood 1 |> addGold -1) Nothing []
        ]


tileLavoriDomestici : Tile
tileLavoriDomestici =
    Tile "Lavori Domestici"
        Gray
        Available
        3
        "assets/img/rounds/lavori_domestici.jpg"
        priceFree
        noWalls
        [ topAction (\r -> r.food > r.actions) (\r -> r |> addFood -r.actions) (Just Furnish) [ 0 ]
        , bottomLeftAction (require .food (>=) 5) (addFood -5) (Just Furnish) [ 1, 2 ]
        , bottomRightAction (require .gold (>=) 1) (addGold -1) (Just Furnish) [ 1, 2 ]
        ]


tileColtivare : Tile
tileColtivare =
    Tile "Coltivare"
        Gray
        Available
        2
        "assets/img/rounds/coltivare.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (\r -> r) (Just (Activate1 True)) [ 0 ]
        , bottomAction alwaysDoable (\r -> r |> addEmmer 2 |> addFlax 1) Nothing [ 1 ]
        ]


tileSottobosco : Tile
tileSottobosco =
    Tile "Sottobosco"
        Gray
        Available
        1
        "assets/img/rounds/sottobosco.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (\r -> r) (Just (Activate1 True)) [ 0 ]
        , bottomAction alwaysDoable (addWood 2) Nothing [ 1 ]
        ]


tileScavare : Tile
tileScavare =
    Tile "Scavare"
        Gray
        Available
        4
        "assets/img/rounds/scavare.jpg"
        priceFree
        noWalls
        [ topLeftAction alwaysDoable (\r -> r) (Just Excavate1) [ 0, 1 ]
        , topRightAction (require .food (>=) 2) (addFood -2) (Just Excavate2) [ 0, 1 ]
        , bottomAction alwaysDoable (addStone 1) Nothing [ 2 ]
        ]


tileArredare : Tile
tileArredare =
    Tile "Arredare"
        Gray
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
    Tile "Costrurire un Muro"
        Gray
        Rock
        7
        "assets/img/rounds/costruire_un_muro.jpg"
        priceFree
        noWalls
        [ topLeftAction alwaysDoable (\r -> r) (Just (Activate1 True)) [ 0 ]
        , thirdAction alwaysDoable (addWood 1) Nothing [ 1, 2 ]
        , fourthAction alwaysDoable (addStone 1) Nothing [ 1, 2 ]
        , bottomAction (require .availableWalls (>) 0) (\r -> r) (Just BuildWall) [ 3 ]
        ]


tileMinare : Tile
tileMinare =
    Tile "Minare"
        Gray
        Rock
        5
        "assets/img/rounds/minare.jpg"
        priceFree
        noWalls
        [ leftAction alwaysDoable (\r -> r) (Just (Activate2 True)) [ 0, 1 ]
        , rightAction alwaysDoable (\r -> r) (Just ExcavateThroughWall) [ 0, 1 ]
        ]


tileDemolireUnMuro : Tile
tileDemolireUnMuro =
    Tile "Demolire un Muro"
        Gray
        Rock
        0
        "assets/img/rounds/demolire_un_muro.jpg"
        priceFree
        noWalls
        -- TODO: resources should be update after wall choice
        [ fullAction alwaysDoable (\r -> r |> addStone 2 |> addFood 3 |> addGold 1) (Just DestroyWall) [ 0 ] ]


tileEspansione : Tile
tileEspansione =
    Tile "Espansione"
        Gray
        Rock
        10
        "assets/img/rounds/espansione.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (\r -> r) (Just Excavate1) [ 0 ]
        , bottomLeftAction (require .food (>=) 5) (addFood -5) (Just Furnish) [ 1, 2 ]
        , bottomRightAction (require .gold (>=) 1) (addGold -1) (Just Furnish) [ 1, 2 ]
        ]


tileSpedizione : Tile
tileSpedizione =
    Tile "Spedizione"
        Gray
        Rock
        9
        "assets/img/rounds/spedizione.jpg"
        priceFree
        noWalls
        [ firstAction (require .wood (>=) 5) (\r -> r |> addWood -5 |> addGold 5) Nothing [ 0, 1, 2 ]
        , secondAction (require .stone (>=) 5) (\r -> r |> addStone -5 |> addGold 5) Nothing [ 0, 1, 2 ]
        , rightAction alwaysDoable (\r -> r) (Just (Activate3 True)) [ 0, 1, 2 ]
        ]


tilePerforare : Tile
tilePerforare =
    Tile "Perforare"
        Gray
        Rock
        8
        "assets/img/rounds/perforare.jpg"
        priceFree
        noWalls
        [ topAction alwaysDoable (\r -> r) (Just (Activate1 True)) [ 0 ]
        , bottomAction (\r -> require .gold (>) r.opponentsGold r) (\r -> r) (Just Excavate1) [ 1 ]
        ]


tileRinnovare : Tile
tileRinnovare =
    Tile "Rinnovare"
        Gray
        Rock
        11
        "assets/img/rounds/rinnovare.jpg"
        priceFree
        noWalls
        [ topAction (require .availableWalls (>) 0) (\r -> r) (Just BuildWall) [ 0 ]
        , bottomAction alwaysDoable (\r -> r) (Just Furnish) [ 1 ]
        ]



-------------------------------------------
-------------EQUIPMENTS--------------------
-------------------------------------------


tileRock : Tile
tileRock =
    Tile "Rock Tile"
        Gray
        Rock
        0
        ""
        priceFree
        (Game.Walls Game.None Game.None Game.None Game.None)
        []


tileEmpty : Tile
tileEmpty =
    Tile "Empty Tile"
        Gray
        Empty
        0
        ""
        priceFree
        (Game.Walls Game.None Game.None Game.None Game.Placed)
        []


tileDungeon : Tile
tileDungeon =
    Tile "Dungeon"
        Blue
        Rock
        11
        "assets/img/equipments/sotterraneo.jpg"
        (priceFree |> priceGold 4 |> priceStone 3)
        (Game.Walls Game.Placed Game.Placed Game.Placed Game.Placed)
        []


tileRettingRoom : Tile
tileRettingRoom =
    Tile "Retting Room"
        Blue
        Rock
        3
        "assets/img/equipments/lavorare_il_lino.jpg"
        (priceFree |> priceStone 1)
        (Game.Walls Game.Placed Game.Optional Game.None Game.Placed)
        []


tileEquipmentRoom : Tile
tileEquipmentRoom =
    Tile "Equipment Room"
        Blue
        Rock
        3
        "assets/img/equipments/equipaggiamenti.jpg"
        (priceFree |> priceWood 2)
        (Game.Walls Game.Placed Game.None Game.None Game.Optional)
        []


tileWoodStoreroom : Tile
tileWoodStoreroom =
    Tile "Wood Storeroom"
        Blue
        Rock
        2
        "assets/img/equipments/deposito_di_legna.jpg"
        (priceFree |> priceStone 1)
        (Game.Walls Game.Placed Game.None Game.None Game.Placed)
        []


tileProspectingSite : Tile
tileProspectingSite =
    Tile "Prospecting Site"
        Blue
        Rock
        5
        "assets/img/equipments/analisi_territoriale.jpg"
        priceFree
        (Game.Walls Game.Placed Game.None Game.None Game.Optional)
        [ bottomAction (\r -> require .food (>) 0 r) (\r -> r |> addFood -1 |> addGold 1) Nothing [ 0 ] ]



-------------------------------------------
-------------ROOM TILES--------------------
-------------------------------------------


tileCaveEntrance : Tile
tileCaveEntrance =
    Tile "Entrata della Cava"
        Orange
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
    Tile "Magazzino"
        Orange
        Rock
        2
        "assets/img/rooms/magazzino.jpg"
        (priceFree |> priceWood 2)
        (Game.Walls Game.Placed Game.Optional Game.None Game.Optional)
        [ fullAction (require .food (>=) 2)
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
    Tile "Shelf"
        Orange
        Available
        3
        "assets/img/rooms/scaffale.jpg"
        (priceFree |> priceWood 1)
        (Game.Walls Game.Placed Game.None Game.None Game.None)
        [ firstAction (require .wood (<) 2) (topWood 2) Nothing [ 0, 1, 2, 3 ]
        , secondAction (require .stone (<) 2) (topStone 2) Nothing [ 0, 1, 2, 3 ]
        , thirdAction (require .emmer (<) 2) (topEmmer 2) Nothing [ 0, 1, 2, 3 ]
        , fourthAction (require .flax (<) 2) (topFlax 2) Nothing [ 0, 1, 2, 3 ]
        ]


tileFoodCorner : Tile
tileFoodCorner =
    Tile "Angolo del Cibo"
        Orange
        Available
        3
        "assets/img/rooms/angolo_del_cibo.jpg"
        (priceFree |> priceStone 1)
        (Game.Walls Game.Placed Game.None Game.None Game.Placed)
        [ fullAction (require .food (<) 3) (topFood 3) Nothing [ 0 ] ]


tileSpinningWheel : Tile
tileSpinningWheel =
    Tile "Filatoio"
        Orange
        Available
        4
        "assets/img/rooms/filatoio.jpg"
        (priceFree |> priceWood 1)
        (Game.Walls Game.Placed Game.None Game.None Game.None)
        [ leftAction (require .flax (>=) 1) (\res -> res |> addFlax -1 |> addGold 1) Nothing [ 0, 1 ]
        , rightAction (require .flax (>=) 3) (\res -> res |> addFlax -3 |> addGold 2) Nothing [ 0, 1 ]
        ]


tileTunnel : Tile
tileTunnel =
    Tile "Tunnel"
        Orange
        Available
        3
        "assets/img/rooms/tunnel.jpg"
        (priceFree |> priceWood 1)
        (Game.Walls Game.None Game.Placed Game.None Game.Placed)
        [ topAction alwaysDoable (addFood 2) Nothing [ 0 ]
        , bottomAction (require .stone (<) 3) (\resources -> resources |> addStone 1 |> minStone 3) Nothing [ 1 ]
        ]


tileAltareSacrificale : Tile
tileAltareSacrificale =
    Tile "Altare Sacrificale"
        Orange
        Rock
        7
        "assets/img/rooms/altare_sacrificale.jpg"
        (priceFree |> priceStone 4)
        (Game.Walls Game.Placed Optional Game.None Optional)
        [ fullAction
            (\res ->
                require .wood (>=) 1 res
                    && require .emmer (>=) 1 res
                    && require .flax (>=) 1 res
                    && require .food (>=) 1 res
            )
            (\res -> res |> addEmmer -1 |> addWood -1 |> addFlax -1 |> addFood -1 |> addGold 3)
            Nothing
            [ 0 ]
        ]


tileBancarella : Tile
tileBancarella =
    Tile "Bancarella"
        Orange
        Rock
        6
        "assets/img/rooms/bancarella.jpg"
        (priceFree |> priceWood 1 |> priceGold 1)
        (Game.Walls Game.Placed Game.Optional Game.None Game.Optional)
        [ leftAction (require .emmer (>=) 5) (\res -> res |> addEmmer -5 |> addGold 4) Nothing [ 0, 1 ]
        , rightAction (require .flax (>=) 5) (\res -> res |> addFlax -5 |> addGold 4) Nothing [ 0, 1 ]
        ]


tileCameraSegreta : Tile
tileCameraSegreta =
    Tile "Camera Segreta"
        Orange
        Rock
        8
        "assets/img/rooms/camera_segreta.jpg"
        (priceFree |> priceWood 2 |> priceStone 1)
        (Game.Walls Game.Placed Game.Placed Game.Placed Game.Placed)
        [ leftAction alwaysDoable (\res -> res |> addFlax 3) Nothing [ 0, 1 ]
        , rightAction alwaysDoable (\res -> res |> addGold 1) Nothing [ 0, 1 ]
        ]


tileCavaInEspansione : Tile
tileCavaInEspansione =
    Tile "Cava in Espansione"
        Orange
        Rock
        8
        "assets/img/rooms/cava_in_espansione.jpg"
        (priceFree |> priceWood 1 |> priceStone 3)
        (Game.Walls Game.Placed Game.Placed Game.None Game.Placed)
        [ fullAction (require .gold (>=) 1) (\res -> res) Nothing [ 0 ] ]


tileDeposito : Tile
tileDeposito =
    Tile "Deposito"
        Orange
        Rock
        6
        "assets/img/rooms/deposito.jpg"
        (priceFree |> priceWood 2 |> priceGold 1)
        (Game.Walls Game.Placed Game.None Game.None Game.Placed)
        [ fullAction alwaysDoable (\res -> res |> addEmmer 1 |> addFlax 1 |> addFood 1) Nothing [ 0 ] ]


tileFiliera : Tile
tileFiliera =
    Tile "Filiera"
        Orange
        Rock
        5
        "assets/img/rooms/filiera.jpg"
        (priceFree |> priceWood 2)
        (Game.Walls Game.Placed Game.None Game.None Game.Placed)
        [ fullAction (require .flax (>=) 2) (\res -> res |> addFlax -2 |> addGold 2 |> addFood 2) Nothing [ 0 ] ]


tileForno : Tile
tileForno =
    Tile "Forno"
        Orange
        Rock
        6
        "assets/img/rooms/forno.jpg"
        (priceFree |> priceWood 1 |> priceStone 2)
        (Game.Walls Game.Placed Game.Placed Game.None Game.Placed)
        [ leftAction (require .emmer (>=) 2) (\res -> res |> addEmmer -2 |> addFood 4 |> addGold 1) Nothing [ 0, 1 ]
        , rightAction (require .emmer (>=) 3) (\res -> res |> addEmmer -3 |> addFood 4 |> addGold 2) Nothing [ 0, 1 ]
        ]


tileMacina : Tile
tileMacina =
    Tile "Macina"
        Orange
        Available
        4
        "assets/img/rooms/macina.jpg"
        (priceFree |> priceStone 1)
        (Game.Walls Game.Placed Game.None Game.None Game.Optional)
        [ leftAction (require .emmer (>=) 1) (\res -> res |> addEmmer -1 |> addFood 3) Nothing [ 0, 1 ]
        , rightAction (require .emmer (>=) 4) (\res -> res |> addEmmer -4 |> addFood 7) Nothing [ 0, 1 ]
        ]


tileGoldMine : Tile
tileGoldMine =
    Tile "Miniera d'Oro"
        Orange
        Rock
        9
        "assets/img/rooms/miniera_d_oro.jpg"
        (priceFree |> priceGold 5)
        (Game.Walls Game.Placed Game.Optional Game.None Game.Placed)
        [ fullAction alwaysDoable (\res -> res |> addGold 1 |> addStone 1) Nothing [ 0 ] ]


tileOfficina : Tile
tileOfficina =
    Tile "Officina"
        Orange
        Rock
        5
        "assets/img/rooms/officina.jpg"
        (priceFree |> priceWood 1 |> priceStone 2)
        (Game.Walls Game.Placed Game.Optional Game.None Game.Placed)
        [ fullAction
            (\res ->
                require .flax (>=) 1 res
                    && require .food (>=) 2 res
            )
            (\res -> res |> addWood -2 |> addFlax -1 |> addGold 3)
            Nothing
            [ 0 ]
        ]


tileSalotto : Tile
tileSalotto =
    Tile "Salotto"
        Orange
        Available
        6
        "assets/img/rooms/salotto.jpg"
        (priceFree |> priceStone 1 |> priceGold 1)
        (Game.Walls Game.Placed Game.Placed Game.None Game.Placed)
        [ fullAction alwaysDoable (\res -> res |> topWood 1 |> topWood 1 |> topEmmer 1 |> topFlax 1 |> topFood 1 |> topGold 1) Nothing [ 0 ] ]


tileLuxuryRoom : Tile
tileLuxuryRoom =
    Tile "Stanza di Lusso"
        Orange
        Rock
        12
        "assets/img/rooms/stanza_di_lusso.jpg"
        (priceFree |> priceGold 7)
        (Game.Walls Game.Placed Game.Placed Game.Optional Game.Placed)
        [ fullAction alwaysDoable (\res -> res |> addGold 1 |> addFlax 1) Nothing [ 0 ] ]


tileStanzaDiSnodo : Tile
tileStanzaDiSnodo =
    Tile "Stanza di Snodo"
        Orange
        Rock
        6
        "assets/img/rooms/stanza_di_snodo.jpg"
        (priceFree |> priceWood 2)
        (Game.Walls Game.None Game.Placed Game.None Game.Placed)
        -- TODO: resources should be update after wall choice
        [ fullAction Resources.atLeastThreeResources (\res -> res |> addGold 2) (Just ChooseResource3) [ 0 ] ]


tileTesoreria : Tile
tileTesoreria =
    Tile "Tesoreria"
        Orange
        Rock
        10
        "assets/img/rooms/tesoreria.jpg"
        (priceFree |> priceGold 3)
        (Game.Walls Game.Placed Game.Placed Game.Placed Game.Placed)
        [ fullAction (require .gold (>=) 3) (\res -> res |> addGold -3 |> addGold 4 |> addFood 1) Nothing [ 0 ] ]


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
