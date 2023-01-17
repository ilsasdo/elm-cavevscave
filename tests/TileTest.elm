module TileTest exposing (..)

import Expect
import Resources exposing (..)
import Test exposing (Test, describe, test)
import Game exposing (..)
import Tiles exposing (require)

suite : Test
suite =
    describe "Tiles functions"
        [ test "no three different resources"
            (\_ ->
                Expect.equal False (atLeastThreeResources (Game.Resources 0 0 0 0 0 0 0 0 0))
            )
        , test "two different resources"
            (\_ ->
                Expect.equal False (atLeastThreeResources (Game.Resources 1 0 1 0 0 0 0 0 0))
            )
        , test "three different resources"
            (\_ ->
                Expect.equal True (atLeastThreeResources (Game.Resources 1 0 1 0 3 0 0 0 0))
            )
        , test "four different resources"
            (\_ ->
                Expect.equal True (atLeastThreeResources (Game.Resources 1 0 1 0 3 5 0 0 0))
            )
        , test "requires at least 3 food new"
            (\_ ->
                Expect.equal True (newRequire .food (>=) 3 (Game.Resources 4 0 0 0 0 0 0 0 0))
            )
        , test "requires at least 3 food new2"
            (\_ ->
                Expect.equal True (newRequire2 .food ((<=) 3) (Game.Resources 4 0 0 0 0 0 0 0 0))
            )
        , test "requires at least 3 food"
            (\_ ->
                Expect.equal True (require ((<=) 3) .food (Game.Resources 4 0 0 0 0 0 0 0 0))
            )
        ]


newRequire : (Resources -> Int) -> (Int -> Int -> Bool) -> Int -> Resources -> Bool
newRequire getter condition qty resources =
    condition (getter resources) qty

newRequire2 : (Resources -> Int) -> (Int -> Bool) -> Resources -> Bool
newRequire2 getter condition resources =
    condition (getter resources)
