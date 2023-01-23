module Game exposing (..)

import Array exposing (Array)


type GameMsg
    = SelectRoomTile Tile
    | DoAction Tile Action
    | SelectWall Int
    | InitPlayerBoard (List Tile)
    | InitRoundTiles (List Tile)
    | PickRoundTile Tile
    | Pass
    | AddToAvailableRooms Tile
    | RemoveFromAvailableRooms Tile
    | WallBuilt
    | WallDestroyed
    | ResourceChosen (Maybe Subphase) (Resources -> Resources)


type RoundPhase
    = NewActionPhase
    | ActionPhase


type TileStatus
    = Available
    | Active
    | Rock
    | Empty


type TileType
    = Orange
    | Blue
    | Gray


type Subphase
    = Escavate1
    | Escavate2
    | Furnish
    | PlaceRoom Tile
    | BuildWall
    | DestroyWall
    | EscavateThroughWall
    | Activate1 Bool
    | Activate2 Bool
    | Activate3 Bool
    | ChooseResource3
    | ChooseResource2
    | ChooseResource1


type Wall
    = Placed
    | Optional
    | None


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


type alias Resources =
    { food : Int
    , wood : Int
    , stone : Int
    , emmer : Int
    , flax : Int
    , gold : Int
    , actions : Int -- actions available in current round are not a proper resource

    -- but are used to pay some actions
    , availableWalls : Int
    , opponentsGold : Int
    }


type alias Walls =
    { north : Wall
    , east : Wall
    , south : Wall
    , west : Wall
    }


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


type alias PlayerBoard =
    { resources : Resources
    , freeAction : Tile
    , rooms : List Tile
    , walls : Array Wall
    , actionTiles : List Tile
    , subphase : Maybe Subphase
    }


roundPhaseToString : RoundPhase -> String
roundPhaseToString phase =
    case phase of
        NewActionPhase ->
            "New Action Phase"

        ActionPhase ->
            "Action Phase"


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

        Just (Activate1 first) ->
            "Activate a Room 1"

        Just (Activate2 first) ->
            "Activate a Room 2"

        Just (Activate3 first) ->
            "Activate a Room 3"

        Just ChooseResource3 ->
            "Choose Three Resources"

        Just ChooseResource2 ->
            "Choose Two Resources"

        Just ChooseResource1 ->
            "Choose One Resource"
