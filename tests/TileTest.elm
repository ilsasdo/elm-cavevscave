module TileTest exposing (..)

import Expect
import Resources exposing (..)
import Test exposing (Test, describe, test)
import Tiles exposing (atLeastThreeResources)


suite : Test
suite =
    describe "Tiles functions"
        [ test "no three different resources"
            (\_ ->
                Expect.equal False (atLeastThreeResources (Resources 0 0 0 0 0 0 0 0 0))
            )
        , test "two different resources"
            (\_ ->
                Expect.equal False (atLeastThreeResources (Resources 1 0 1 0 0 0 0 0 0))
            )
        , test "three different resources"
            (\_ ->
                Expect.equal True (atLeastThreeResources (Resources 1 0 1 0 3 0 0 0 0))
            )
        , test "four different resources"
            (\_ ->
                Expect.equal True (atLeastThreeResources (Resources 1 0 1 0 3 5 0 0 0))
            )
        ]
