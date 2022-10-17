module Main exposing (main)

import Array
import Browser
import Debug exposing (toString)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import PlayerBoard exposing (PlayerBoard, isRoomSelectable, restorePlayerNextRound, restorePlayerPass, restoreRoom, viewBoard)
import Random
import Random.List
import Resources exposing (Resources)
import Tiles exposing (Action, Msg(..), Subphase(..), Tile, TileStatus(..), tileAltareSacrificale, tileAnalisiTerritoriale, tileArredare, tileBancarella, tileCameraSegreta, tileCavaInEspansione, tileCaveEntrance, tileColtivare, tileCostruireUnMuro, tileDemolireUnMuro, tileDeposito, tileDepositoDiLegna, tileEmpty, tileEquipaggiamenti, tileEspansione, tileFiliera, tileFoodCorner, tileForno, tileGoldMine, tileLavorareIlLino, tileLavoriDomestici, tileLuxuryRoom, tileMacina, tileMinare, tileOfficina, tilePerforare, tileRinnovare, tileSalotto, tileScavare, tileShelf, tileSotterraneo, tileSottobosco, tileSpedizione, tileSpinningWheel, tileStanzaDiSnodo, tileTesoreria, tileTunnel, tileWarehouse, updateStatus, viewTile)
import Walls


type alias Game =
    { player1 : PlayerBoard
    , player2 : PlayerBoard
    , round : Int -- starts with 1 ends with 8
    , actions : Int -- 2 actions for rounds 1,2,3. 3 actions for rounds 4,5,6,7. 4 actions for round 8
    , currentPlayer : Int -- 1 or 2
    , phase : RoundPhase
    , actionTiles : List Tile
    , availableRooms : List Tile
    , availableWalls : Int
    }


type Msg
    = InitPlayerBoard (List Tile)
    | InitRoundTiles (List Tile)
    | PlayerMsg Tiles.Msg
    | PickRoundTile Tile
    | Pass


type RoundPhase
    = NewActionPhase
    | ActionPhase


main =
    Browser.element { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }


init : () -> ( Game, Cmd Msg )
init _ =
    ( Game newBoard newBoard 1 2 1 NewActionPhase [] newAvailableRooms 7
    , setupRandomTiles
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
        , tileAnalisiTerritoriale
        , tileSotterraneo
        , tileEquipaggiamenti
        , tileLavorareIlLino
        , tileDepositoDiLegna
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
    )


setupRandomTiles : List Tile -> List Tile -> List Tile -> List Tile -> List Tile -> Cmd Msg
setupRandomTiles rooms round1Tiles round2Tiles round3Tiles round4Tiles =
    Cmd.batch
        [ Random.generate InitPlayerBoard (Random.List.shuffle rooms)
        , Random.generate InitRoundTiles (Random.List.shuffle round4Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round3Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round2Tiles)
        , Random.generate InitRoundTiles (Random.List.shuffle round1Tiles)
        ]


newBoard : PlayerBoard
newBoard =
    PlayerBoard (Resources 1 1 1 1 1 1 1) [] (Array.repeat 14 Walls.None) [] Nothing


newAvailableRooms : List Tile
newAvailableRooms =
    [ tileShelf
    , tileSpinningWheel
    , tileMacina
    , tileSalotto
    , tileTunnel
    , tileFoodCorner
    ]


update : Msg -> Game -> ( Game, Cmd Msg )
update msg ({ player1, player2 } as game) =
    let
        activePlayer =
            currentPlayer game
    in
    case Debug.log "msg: " msg of
        InitRoundTiles tiles ->
            ( { game
                | actionTiles =
                    List.indexedMap
                        (\i ->
                            \t ->
                                if i < 5 then
                                    { t | status = Available }

                                else
                                    t
                        )
                        (game.actionTiles ++ tiles)
              }
            , Cmd.none
            )

        InitPlayerBoard rooms ->
            ( { game
                | player1 = { player1 | rooms = List.take 9 rooms |> PlayerBoard.init }
                , player2 = { player2 | rooms = List.drop 9 rooms |> List.take 9 |> PlayerBoard.init }
              }
            , Cmd.none
            )

        PlayerMsg tileMsg ->
            let
                ( player, playerMsg ) =
                    PlayerBoard.update tileMsg activePlayer
            in
            case playerMsg of
                PlayerBoard.NewTileAvailable newTile ->
                    ( game |> updateCurrentPlayer player |> addNewAvailableRoom newTile, Cmd.none )

                PlayerBoard.WallBuilt ->
                    ( { game | availableWalls = game.availableWalls - 1 } |> updateCurrentPlayer player, Cmd.none )

                PlayerBoard.WallDestroyed ->
                    ( { game | availableWalls = game.availableWalls + 1 } |> updateCurrentPlayer player, Cmd.none )

                PlayerBoard.None ->
                    ( game |> updateCurrentPlayer player, Cmd.none )

        Pass ->
            ( pass game, Cmd.none )

        PickRoundTile tile ->
            ( { game
                | phase = ActionPhase
                , actionTiles = Tiles.updateStatus tile Tiles.Empty game.actionTiles
              }
                |> updateCurrentPlayer { activePlayer | actionTiles = { tile | status = Tiles.Active } :: activePlayer.actionTiles }
            , Cmd.none
            )


pass : Game -> Game
pass game =
    if List.length game.player1.actionTiles == game.actions && List.length game.player2.actionTiles == game.actions then
        nextRound game

    else
        game
            |> updateCurrentPlayer (restorePlayerPass (currentPlayer game))
            |> nextPlayer


addNewAvailableRoom : Tile -> Game -> Game
addNewAvailableRoom tile game =
    { game | availableRooms = game.availableRooms ++ [ { tile | status = Available } ] }


nextRound : Game -> Game
nextRound game =
    let
        round =
            game.round + 1

        -- 2 actions for rounds 1,2,3. 3 actions for rounds 4,5,6,7. 4 actions for round 8
        actions =
            if round < 4 then
                2

            else if round < 8 then
                3

            else
                4

        -- restore the action tiles and flip the next round tile
        actionTiles =
            List.indexedMap
                (\i ->
                    \r ->
                        if i <= (round + 3) then
                            { r | status = Available }

                        else
                            r
                )
                game.actionTiles
    in
    { game
        | phase = NewActionPhase
        , round = round
        , actions = actions
        , actionTiles = actionTiles
        , player1 = restorePlayerNextRound game.player1 actions
        , player2 = restorePlayerNextRound game.player2 actions
    }


nextPlayer : Game -> Game
nextPlayer game =
    { game
        | phase = NewActionPhase
        , currentPlayer =
            if game.currentPlayer == 1 then
                2

            else
                1
    }


view : Game -> Html Msg
view game =
    div [ class "container" ]
        [ viewStatusBar game, viewActionTiles game, viewMain game ]


viewStatusBar : Game -> Html Msg
viewStatusBar game =
    div [ class "statusbar" ]
        [ Html.button [ onClick Pass ] [ text "Pass" ]
        , div []
            [ text
                ("Round: "
                    ++ toString game.round
                    ++ " || Player "
                    ++ toString game.currentPlayer
                    ++ " || Actions: "
                    ++ (game |> currentPlayer |> .actionTiles |> List.length |> toString)
                    ++ "/"
                    ++ toString game.actions
                    ++ " || Phase: "
                    ++ toString game.phase
                    ++ " || Subphase: "
                    ++ (game |> currentPlayer |> .subphase |> toString)
                    ++ " || Available Walls: "
                    ++ (game.availableWalls |> toString)
                )
            ]
        ]


currentPlayer : Game -> PlayerBoard
currentPlayer game =
    if game.currentPlayer == 1 then
        game.player1

    else
        game.player2


updateCurrentPlayer : PlayerBoard -> Game -> Game
updateCurrentPlayer player game =
    if game.currentPlayer == 1 then
        { game | player1 = player }

    else
        { game | player2 = player }


viewActionTiles : Game -> Html Msg
viewActionTiles game =
    div [ class "actiontiles" ]
        (List.map (viewActionTile game) game.actionTiles)


viewActionTile : Game -> Tile -> Html Msg
viewActionTile game tile =
    if game.phase == NewActionPhase && tile.status == Available then
        Html.map mapToPickRoundTile (viewTile [ class "actiontile pick", onClick (SelectRoomTile tile) ] (currentPlayer game).resources tile)

    else
        Html.map PlayerMsg (viewTile [ class "actiontile" ] game.player1.resources tile)


mapToPickRoundTile msg =
    case msg of
        Tiles.SelectRoomTile tile ->
            PickRoundTile tile

        _ ->
            PlayerMsg msg


viewMain : Game -> Html Msg
viewMain game =
    Html.map PlayerMsg
        (div [ class "mainboard" ]
            [ viewBoard game.player1
            , viewAvailableRooms (currentPlayer game) game.availableRooms
            , viewBoard game.player2
            ]
        )


viewAvailableRooms : PlayerBoard -> List Tile -> Html Tiles.Msg
viewAvailableRooms player rooms =
    div [ class "availablerooms" ] (List.map (viewAvailableRoom player) rooms)


viewAvailableRoom : PlayerBoard -> Tile -> Html Tiles.Msg
viewAvailableRoom player room =
    if player.subphase == Just Furnish && isRoomSelectable player room then
        viewTile [ class "availableroom pick", onClick (SelectRoomTile room) ] player.resources room

    else
        viewTile [ class "availableroom" ] player.resources room


type alias PlayerMove =
    List Msg


type alias Node =
    { game : Game
    , moves : List PlayerMove
    , value : Int
    }


alphaBeta : Node -> Int -> Int -> Int -> Bool -> Node
alphaBeta node depth a b maximizingPlayer =
    if depth == 0 || isTerminalNode node then
        node

    else if maximizingPlayer then
        eachNodeMax (List.head node.moves) (Node node.game (List.drop 1 node.moves) -9999999) (depth - 1) a b False

    else
        eachNodeMin (List.head node.moves) (Node node.game (List.drop 1 node.moves) 9999999) (depth - 1) a b True


eachNodeMax : Maybe PlayerMove -> Node -> Int -> Int -> Int -> Bool -> Node
eachNodeMax move node depth a b maximizingPlayer =
    case move of
        Nothing ->
            node

        Just mv ->
            let
                updatedNode =
                    updateWithMoves mv node.game

                abNode =
                    alphaBeta updatedNode depth a b maximizingPlayer

                newValue =
                    max node.value abNode.value
            in
            if newValue >= b then
                abNode

            else
                eachNodeMax (List.head node.moves) (Node node.game (List.drop 1 node.moves) newValue) depth (max a newValue) b maximizingPlayer



{-

   value := +∞
   for each child of node do
       value := min(value, alphabeta(child, depth − 1, α, β, TRUE))
       if value ≤ α then
           break (* α cutoff *)
       β := min(β, value)
   return value

-}


eachNodeMin : Maybe PlayerMove -> Node -> Int -> Int -> Int -> Bool -> Node
eachNodeMin move node depth a b maximizingPlayer =
    case move of
        Nothing ->
            node

        Just mv ->
            let
                updatedNode =
                    updateWithMoves mv node.game

                abNode =
                    alphaBeta updatedNode depth a b maximizingPlayer

                newValue =
                    min node.value abNode.value
            in
            if newValue <= a then
                abNode

            else
                eachNodeMin (List.head node.moves) (Node node.game (List.drop 1 node.moves) newValue) depth a (min b newValue) maximizingPlayer


updateWithMoves : List Msg -> Game -> Node
updateWithMoves moves game =
    let
        msg =
            List.head moves
    in
    case msg of
        Just m ->
            updateWithMoves (List.drop 1 moves) (update m game |> Tuple.first)

        Nothing ->
            Node game (calculatePlayerMoves game (currentPlayer game)) (calculateGameValue game)


isTerminalNode: Node -> Bool
isTerminalNode node =
    if node.game.round > 8 then
        True

    else
        False


calculateGameValue game =
    0


calculatePlayerMoves: Game -> PlayerBoard -> List PlayerMove
calculatePlayerMoves game player =
    game.actionTiles
        |> List.filter (\t -> t.status == Available)
        |> List.sortBy (\t -> -t.score)
        |> List.map (chooseActionTile game player)
        |> List.foldl (++) []


chooseActionTile: Game -> PlayerBoard -> Tile -> List PlayerMove
chooseActionTile game player tile =
    tile.actions
    |> List.filter (\a -> a.isDoable player.resources)
    |> List.map (playAction game player tile [PickRoundTile tile])
    |> List.foldl (++) []


playAction: Game -> PlayerBoard -> Tile -> PlayerMove -> Action -> List PlayerMove
playAction game player tile moves action =
    case action.subphase of

        Nothing ->
            [moves]

        _ ->
            [moves]

        --Just Escavate1 ->
        --    List.filter (\t -> )
        --Just Escavate2 ->
        --Just Furnish ->
        --Just PlaceRoom r ->
        --Just BuildWall ->
        --Just DestroyWall ->
        --Just EscavateThroughWall ->
        --Just Activate1 ->
        --Just Activate2 ->
        --Just Activate3 ->

