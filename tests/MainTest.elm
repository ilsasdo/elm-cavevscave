module MainTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (emptyGame, playTile)
import Test exposing (..)
import Tiles exposing (tileColtivare)


suite : Test
suite =
    describe "Calculating available moves"
        [ test "Playing tileColtivare in an empty board should output only one move available"
            (\_ ->
                let
                    game =
                        emptyGame

                    node =
                        Main.Node game 0 []
                in
                Expect.equal 1 (playTile node tileColtivare tileColtivare.actions |> List.length)
            )
        ]
