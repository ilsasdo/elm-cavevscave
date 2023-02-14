

import Debug exposing (toString)
import Game exposing (Game, GameMsg(..), RoundPhase(..), TileStatus(..))
import PlayerBoard exposing (isRoomSelectable)

type alias PlayerMove =
    List GameMsg


type alias Node =
    { game : Game
    , value : Int
    , move : PlayerMove
    }


emptyGame =
    Game PlayerBoard.emptyBoard PlayerBoard.emptyBoard 0 0 0 NewActionPhase [] []


playAI game =
    let
        rootNode =
            Node game 0 []

        node =
            alphaBeta rootNode 1 9999999 -9999999 True
    in
    playAIMoves node.move game

nodeToString : Node -> String
nodeToString node =
    "Node (" ++ toString node.value ++ "), moves: " ++ playerMoveToString node.move


nodeValues : List Node -> String
nodeValues nodes =
    nodes
        |> List.map .value
        |> List.map toString
        |> String.join ", "


playerMoveToString : List GameMsg -> String
playerMoveToString playerMove =
    playerMove
        |> List.map msgToString
        |> String.join ", "


msgToString : GameMsg -> String
msgToString msg =
    case msg of
        PickRoundTile t ->
            "PickRoundTile(" ++ t.title ++ ")"

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


playAIMoves : List GameMsg -> Game -> ( Game, Cmd GameMsg )
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


playMove : Node -> GameMsg -> Node
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
                |> List.filter (PlayerBoard.isExcavatable player)
                |> List.map (\t -> playMove node (PlayerMsg (SelectRoomTile t)))
                |> List.map playRoundTileAction
                |> List.foldl (++) []

        Just Escavate2 ->
            player.rooms
                |> List.filter (\t -> t.status == Rock)
                |> List.filter (PlayerBoard.isExcavatable player)
                |> List.map (\t -> playMove node (PlayerMsg (SelectRoomTile t)))
                |> List.map playRoundTileAction
                |> List.foldl (++) []

        -- TODO: handle through wall special case here
        Just EscavateThroughWall ->
            player.rooms
                |> List.filter (\t -> t.status == Rock)
                |> List.filter (PlayerBoard.isExcavatable player)
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
