module Game exposing (..)

import Array exposing (Array)
import Stack exposing (Stack)


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
    | ResourceChosen (Resources -> Resources)


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
    = NewActionPhase
    | ActionPhase
    | Excavate
    | Furnish
    | PlaceRoom Tile
    | BuildWall
    | DestroyWall
    | ExcavateThroughWall
    | Activate
    | ChooseResource


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
    , subphase : List Subphase
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
    , actionTiles : List Tile
    , availableRooms : List Tile
    , availableWalls : Int
    , stack : Stack Subphase
    }


type alias PlayerBoard =
    { resources : Resources
    , freeAction : Tile
    , rooms : List Tile
    , walls : Array Wall
    , actionTiles : List Tile
    , active : Bool
    }


subphaseToString : Maybe Subphase -> String
subphaseToString subphase =
    case subphase of
        Just NewActionPhase ->
            "New Action Phase"

        Just ActionPhase ->
            "Action Phase"

        Nothing ->
            ""

        Just Excavate ->
            "Escavate 1"

        Just Furnish ->
            "Furnish"

        Just (PlaceRoom tile) ->
            "PlaceRoom " ++ tile.title

        Just BuildWall ->
            "Build a Wall"

        Just DestroyWall ->
            "Destroy a Wall"

        Just ExcavateThroughWall ->
            "Escavate through a Wall"

        Just Activate ->
            "Activate a Room 1"

        Just ChooseResource ->
            "Choose One Resource"
