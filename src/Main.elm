module Main exposing (Node, emptyGame, main, playTile)

import Array
import Browser
import Debug exposing (toString)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import PlayerBoard exposing (PlayerBoard, isRoomSelectable, restorePlayerNextRound, restorePlayerPass, viewBoard)
import Random
import Random.List
import Resources exposing (Resources)
import Tiles exposing (Action, Msg(..), Subphase(..), Tile, TileStatus(..), subphaseToString, tileAltareSacrificale, tileAnalisiTerritoriale, tileArredare, tileBancarella, tileCameraSegreta, tileCavaInEspansione, tileColtivare, tileCostruireUnMuro, tileDemolireUnMuro, tileDeposito, tileDepositoDiLegna, tileEquipaggiamenti, tileEspansione, tileFiliera, tileFoodCorner, tileForno, tileFreeAction, tileGoldMine, tileLavorareIlLino, tileLavoriDomestici, tileLuxuryRoom, tileMacina, tileMinare, tileOfficina, tilePerforare, tileRinnovare, tileSalotto, tileScavare, tileShelf, tileSotterraneo, tileSottobosco, tileSpedizione, tileSpinningWheel, tileStanzaDiSnodo, tileTesoreria, tileTunnel, tileWarehouse, viewTile)
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


emptyGame =
    Game emptyBoard emptyBoard 0 0 0 NewActionPhase [] [] 0


emptyBoard =
    PlayerBoard (Resources 0 0 0 0 0 0 0 7) tileFreeAction [] (Array.fromList []) [] Nothing


type Msg
    = InitPlayerBoard (List Tile)
    | InitRoundTiles (List Tile)
    | PlayerMsg Tiles.Msg
    | PickRoundTile Tile
    | Pass
    | PlayAI


type RoundPhase
    = NewActionPhase
    | ActionPhase


phaseToString phase =
    case phase of
        NewActionPhase ->
            "New Action Phase"

        ActionPhase ->
            "Action Phase"


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
    PlayerBoard (Resources 1 1 1 1 1 1 1 7) tileFreeAction [] (Array.repeat 14 Walls.None) [] Nothing


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
    case msg of
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
                | player1 =
                    { player1
                        | rooms = List.take 9 rooms |> PlayerBoard.init
                        , freeAction = Tiles.setStatus Active player1.freeAction
                    }
                , player2 =
                    { player2
                        | rooms = List.drop 9 rooms |> List.take 9 |> PlayerBoard.init
                        , freeAction = Tiles.setStatus Available player1.freeAction
                    }
              }
            , Cmd.none
            )

        PlayerMsg tileMsg ->
            let
                ( player, playerMsg ) =
                    PlayerBoard.update tileMsg activePlayer
            in
            case playerMsg of
                PlayerBoard.AddToAvailableRooms tile ->
                    ( game |> updateCurrentPlayer player |> addToAvailableRooms tile, Cmd.none )

                PlayerBoard.RemoveFromAvailableRooms tile ->
                    ( game |> updateCurrentPlayer player |> removeFromAvailableRooms tile, Cmd.none )

                PlayerBoard.WallBuilt ->
                    ( game |> updateCurrentPlayer player |> updateAvailableWalls -1, Cmd.none )

                PlayerBoard.WallDestroyed ->
                    ( game |> updateCurrentPlayer player |> updateAvailableWalls 1, Cmd.none )

                PlayerBoard.None ->
                    ( game |> updateCurrentPlayer player, Cmd.none )

        Pass ->
            ( pass game, Cmd.none )

        PlayAI ->
            let
                rootNode =
                    Node game 0 []

                node =
                    alphaBeta rootNode 1 9999999 -9999999 True
            in
            playAIMoves node.move game

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
            |> activatePlayer


activatePlayer : Game -> Game
activatePlayer game =
    let
        player =
            currentPlayer game
    in
    updateCurrentPlayer { player | freeAction = player.freeAction |> PlayerBoard.restoreTile |> Tiles.setStatus Active  } game


updateAvailableWalls : Int -> Game -> Game
updateAvailableWalls qty ({ player1, player2 } as game) =
    { game
        | availableWalls = game.availableWalls + qty
        , player1 = updateAvailableWallsResource qty player1
        , player2 = updateAvailableWallsResource qty player2
    }


updateAvailableWallsResource : Int -> PlayerBoard -> PlayerBoard
updateAvailableWallsResource qty ({ resources } as player) =
    { player | resources = { resources | availableWalls = resources.availableWalls + qty } }


addToAvailableRooms : Tile -> Game -> Game
addToAvailableRooms tile game =
    { game | availableRooms = game.availableRooms ++ [ { tile | status = Available } ] }


removeFromAvailableRooms : Tile -> Game -> Game
removeFromAvailableRooms tile game =
    { game | availableRooms = List.filter (\r -> r.title /= tile.title) game.availableRooms }


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
        , Html.button [ onClick PlayAI ] [ text "Play AI" ]
        , div []
            [ text
                ("Round: "
                    ++ String.fromInt game.round
                    ++ " || Player "
                    ++ String.fromInt game.currentPlayer
                    ++ " || Actions: "
                    ++ (game |> currentPlayer |> .actionTiles |> List.length |> String.fromInt)
                    ++ "/"
                    ++ String.fromInt game.actions
                    ++ " || Phase: "
                    ++ phaseToString game.phase
                    ++ " || Subphase: "
                    ++ (game |> currentPlayer |> .subphase |> subphaseToString)
                    ++ " || Available Walls: "
                    ++ (game.availableWalls |> String.fromInt)
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
    , value : Int
    , move : PlayerMove
    }


nodeToString : Node -> String
nodeToString node =
    "Node (" ++ toString node.value ++ "), moves: " ++ playerMoveToString node.move


nodeValues : List Node -> String
nodeValues nodes =
    nodes
        |> List.map .value
        |> List.map toString
        |> String.join ", "


playerMoveToString : List Msg -> String
playerMoveToString playerMove =
    playerMove
        |> List.map msgToString
        |> String.join ", "


msgToString : Msg -> String
msgToString msg =
    case msg of
        PickRoundTile t ->
            "PickRoundTile(" ++ t.title ++ ")"

        PlayerMsg playerMsg ->
            case playerMsg of
                SelectRoomTile t1 ->
                    "SelectRoomTile(" ++ t1.title ++ ")"

                DoAction t1 action ->
                    "DoAction(" ++ t1.title ++ ", " ++ toString action.subphase ++ ")"

                SelectWall w ->
                    "SelectWall(" ++ toString w ++ ")"

        _ ->
            toString msg


alphaBeta : Node -> Int -> Int -> Int -> Bool -> Node
alphaBeta node depth a b maximizingPlayer =
    if depth == 0 || isTerminalNode node then
        node

    else
        let
            nodes =
                calculatePlayerMoves node.game

            print1 =
                Debug.log "AlphaBeta" ("max=" ++ toString maximizingPlayer ++ ", depth=" ++ toString depth ++ ", a=" ++ toString a ++ ", b=" ++ toString b ++ ", nodeCount=" ++ (List.length nodes |> toString) ++ ", nodeValues: " ++ nodeValues nodes)
        in
        if maximizingPlayer then
            eachNodeMax nodes (depth - 1) a b False

        else
            eachNodeMin nodes (depth - 1) a b True


emptyNode =
    Node emptyGame 0 []


eachNodeMax : List Node -> Int -> Int -> Int -> Bool -> Node
eachNodeMax nodes depth a b maximizingPlayer =
    let
        node =
            List.head nodes |> Maybe.withDefault emptyNode
    in
    if List.length nodes == 1 then
        node

    else
        let
            abNode =
                alphaBeta node depth a b maximizingPlayer

            newValue =
                max node.value abNode.value
        in
        if newValue >= b then
            abNode

        else
            eachNodeMax (List.drop 1 nodes) depth (max a newValue) b maximizingPlayer


eachNodeMin : List Node -> Int -> Int -> Int -> Bool -> Node
eachNodeMin nodes depth a b maximizingPlayer =
    let
        node =
            List.head nodes |> Maybe.withDefault emptyNode
    in
    if List.length nodes == 1 then
        node

    else
        let
            abNode =
                alphaBeta node depth a b maximizingPlayer

            newValue =
                max node.value abNode.value
        in
        if newValue <= a then
            abNode

        else
            eachNodeMin (List.drop 1 nodes) depth a (min b newValue) maximizingPlayer


playAIMoves : List Msg -> Game -> ( Game, Cmd Msg )
playAIMoves moves game =
    let
        msg =
            List.head moves
    in
    case msg of
        Just m ->
            playAIMoves (List.drop 1 moves) (update m game |> Tuple.first)

        Nothing ->
            ( game, Cmd.none )


isTerminalNode : Node -> Bool
isTerminalNode node =
    if node.game.round > 8 then
        True

    else
        False


calculatePlayerMoves : Game -> List Node
calculatePlayerMoves game =
    game.actionTiles
        |> List.filter (\t -> t.status == Available)
        |> List.sortBy (\t -> -t.score)
        |> List.map (\roundTile -> playTile (playMove (Node game 0 []) (PickRoundTile roundTile)) roundTile roundTile.actions)
        |> List.foldl (++) []
        |> List.sortBy (\n -> -n.value)
        |> List.map passGame


passGame : Node -> Node
passGame node =
    playMove node Pass


playMove : Node -> Msg -> Node
playMove node msg =
    let
        updatedGame =
            update msg node.game |> Tuple.first
    in
    Node updatedGame node.value (node.move ++ [ msg ])


playMoves : Node -> PlayerMove -> Node
playMoves node moves =
    let
        move =
            List.head moves
    in
    case move of
        Just msg ->
            playMoves (playMove node msg) (List.drop 1 moves)

        Nothing ->
            node


playTile : Node -> Tile -> List Action -> List Node
playTile node tile actions =
    let
        action =
            List.head actions

        player =
            currentPlayer node.game
    in
    case action of
        Just a ->
            if a.isDoable player.resources then
                let
                    newNode =
                        playMove node (PlayerMsg (DoAction tile a))

                    playedActions =
                        playRoundTileAction newNode
                in
                if List.length playedActions == 0 then
                    playTile newNode tile (List.drop 1 actions)

                else
                    playedActions
                        |> List.map (\n -> playTile n tile (List.drop 1 actions))
                        |> List.foldl (++) []

            else
                playTile node tile (List.drop 1 actions)

        Nothing ->
            [ node ]


playRoundTileAction : Node -> List Node
playRoundTileAction node =
    let
        player =
            currentPlayer node.game
    in
    case player.subphase of
        Nothing ->
            [ Node node.game (calculateGameValue node.game) node.move ]

        Just Escavate1 ->
            player.rooms
                |> List.filter (\t -> t.status == Rock)
                |> List.filter (PlayerBoard.isReachableRoom player)
                |> List.map (\t -> playMove node (PlayerMsg (SelectRoomTile t)))
                |> List.map playRoundTileAction
                |> List.foldl (++) []

        Just Escavate2 ->
            player.rooms
                |> List.filter (\t -> t.status == Rock)
                |> List.filter (PlayerBoard.isReachableRoom player)
                |> List.map (\t -> playMove node (PlayerMsg (SelectRoomTile t)))
                |> List.map playRoundTileAction
                |> List.foldl (++) []

        -- TODO: handle through wall special case here
        Just EscavateThroughWall ->
            player.rooms
                |> List.filter (\t -> t.status == Rock)
                |> List.filter (PlayerBoard.isReachableRoom player)
                |> List.map (\t -> playMove node (PlayerMsg (SelectRoomTile t)))
                |> List.map playRoundTileAction
                |> List.foldl (++) []

        Just Furnish ->
            node.game.availableRooms
                |> List.filter (PlayerBoard.isRoomSelectable player)
                |> List.map (\t -> playMove node (PlayerMsg (SelectRoomTile t)))
                |> List.map playRoundTileAction
                |> List.foldl (++) []

        Just (PlaceRoom tileToPlace) ->
            player.rooms
                |> List.filter (\t -> t.status == Empty && Walls.matches t.walls tileToPlace.walls)
                |> List.map (\t -> playMove node (PlayerMsg (SelectRoomTile t)))
                |> List.map playRoundTileAction
                |> List.foldl (++) []

        Just BuildWall ->
            player.walls
                |> Array.toIndexedList
                |> List.filter (\( i, w ) -> w == Walls.None)
                |> List.map (\( i, w ) -> playMove node (PlayerMsg (Tiles.SelectWall i)))
                |> List.map playRoundTileAction
                |> List.foldl (++) []

        Just DestroyWall ->
            player.walls
                |> Array.toIndexedList
                |> List.filter (\( i, w ) -> w == Walls.Placed)
                |> List.map (\( i, w ) -> playMove node (PlayerMsg (Tiles.SelectWall i)))
                |> List.map playRoundTileAction
                |> List.foldl (++) []

        Just Activate1 ->
            player.rooms
                |> List.filter (\t -> t.status == Available)
                |> List.map (activateTile node)
                |> List.foldl (++) []
                |> List.map playRoundTileAction
                |> List.foldl (++) []

        Just Activate2 ->
            player.rooms
                |> List.filter (\t -> t.status == Available)
                |> List.map (activateTile node)
                |> List.foldl (++) []
                |> List.map playRoundTileAction
                |> List.foldl (++) []

        Just Activate3 ->
            player.rooms
                |> List.filter (\t -> t.status == Available)
                |> List.map (activateTile node)
                |> List.foldl (++) []
                |> List.map playRoundTileAction
                |> List.foldl (++) []


activateTile : Node -> Tile -> List Node
activateTile node tile =
    let
        n =
            playMove node (PlayerMsg (Tiles.SelectRoomTile tile))
    in
    tile.actions
        |> permutations
        |> List.map (applyActions n tile)


applyActions : Node -> Tile -> List Action -> Node
applyActions node tile actions =
    playMoves node (List.map (\a -> PlayerMsg (DoAction tile a)) actions)


permutations : List Action -> List (List Action)
permutations actions =
    actions
        |> List.map (\a -> [ a ] ++ nonDisabledActions actions a.disableActions)


nonDisabledActions : List Action -> List Int -> List Action
nonDisabledActions actions disabledActions =
    actions
        |> List.indexedMap Tuple.pair
        |> List.filter (\( i, a ) -> not (List.member i disabledActions))
        |> List.map Tuple.second


calculateGameValue : Game -> Int
calculateGameValue game =
    let
        player =
            currentPlayer game
    in
    (player.resources.gold * 10)
        + (player.resources.emmer * 2)
        + (player.resources.flax * 2)
        + (player.resources.food * 3)
        + (player.resources.stone * 2)
        + (player.resources.wood * 2)
        + (player.rooms
            |> List.filter (\r -> r.status == Available || r.status == Active)
            |> List.map .score
            |> List.foldl (+) 0
            |> (*) 100
          )
        + (player.rooms
            |> List.filter (\r -> r.status == Empty)
            |> List.map .score
            |> List.foldl (+) 0
            |> (*) 5
          )
        + (game.availableRooms
            |> List.filter (isRoomSelectable player)
            |> List.map .score
            |> List.foldl (+) 0
            |> (*) 6
          )
