module Walls exposing (..)


import Array exposing (Array)
import Game as Wall exposing (Wall(..), Walls)


get: Int -> Array Wall -> Wall
get index walls =
    case (Array.get index walls) of
        Just wall ->
            wall

        Nothing ->
            Optional


matches: Walls -> Walls -> Bool
matches caveWall roomWall =
    matchesAux caveWall roomWall 4


matchesAux: Walls -> Walls -> Int -> Bool
matchesAux caveWall roomWall rotation =
    if rotation == 0 then
        False

    else
        if matchSides caveWall roomWall then
            True

        else
            matchesAux caveWall (rotateClockwise roomWall) (rotation - 1)


rotateClockwise: Walls -> Walls
rotateClockwise walls =
    Walls walls.west walls.north walls.east walls.south


matchSides: Walls -> Walls -> Bool
matchSides caveWall roomWall =
     matchSide caveWall .north roomWall &&
     matchSide caveWall .east roomWall &&
     matchSide caveWall .south roomWall &&
     matchSide caveWall .west roomWall


matchSide: Walls -> (Walls -> Wall) -> Walls -> Bool
matchSide r1 side r2 =
    case (side r2) of
        Wall.Placed ->
            (side r1) == Placed || (side r1) == Wall.Optional

        Wall.Optional ->
            True

        Wall.None ->
            (side r1) == Wall.None || (side r1) == Wall.Optional
