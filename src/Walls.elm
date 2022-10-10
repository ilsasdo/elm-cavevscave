module Walls exposing (..)


import Array exposing (Array)
import Debug exposing (toString)
import Html exposing (Html, div)
import Html.Attributes exposing (class)

type Msg =
    Destroy Int
    | Build Int

type Wall
    = Placed
    | Optional
    | None


type alias Walls =
    { north : Wall
    , east : Wall
    , south : Wall
    , west : Wall
    }


get: Int -> Array Wall -> Wall
get index walls =
    case (Array.get index walls) of
        Just wall ->
            wall

        Nothing ->
            Optional


matches: Walls -> Walls -> Bool
matches caveWall roomWall =
    (roomWall, False)
    |> matchSides caveWall
    |> rotateClockwise
    |> matchSides caveWall
    |> rotateClockwise
    |> matchSides caveWall
    |> rotateClockwise
    |> matchSides caveWall
    |> Tuple.second


rotateClockwise: (Walls, Bool) -> (Walls, Bool)
rotateClockwise (walls, match) =
    (Walls walls.west walls.north walls.east walls.south, match)


matchSides: Walls -> (Walls, Bool) -> (Walls, Bool)
matchSides caveWall (roomWall, match) =
    -- if I already found a configuration that matches, skip all the other configs.
    if match == True then
        (roomWall, True)

    else -- otherwise check this configuration
        (roomWall, True)
            |> matchSide caveWall .north
            |> matchSide caveWall .east
            |> matchSide caveWall .south
            |> matchSide caveWall .west

matchSide: Walls -> (Walls -> Wall) -> (Walls, Bool) -> (Walls, Bool)
matchSide cave side (room, match) =
    -- if one side of the configuration doesn't match, return false for this configuration.
    if match == False then
        (room, False)

    else
        case (side room) of
            Placed ->
                (room, (side cave) == Placed)

            Optional ->
                (room, True)

            None ->
                (room, (side cave) == None)


viewWalls : Array Wall -> List (Html msg)
viewWalls walls =
    walls
    |> Array.indexedMap viewWall
    |> Array.toList


viewWall : Int -> Wall -> Html msg
viewWall index wall =
    case wall of
        Placed ->
            div [ class ("wall placed wall-" ++ toString index) ] []

        Optional ->
            div [ class ("wall placed wall-" ++ toString index) ] []

        None ->
            div [ class ("wall available wall-" ++ toString index) ] []