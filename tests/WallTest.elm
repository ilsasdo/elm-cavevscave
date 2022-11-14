module WallTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Walls exposing (Wall(..))



-- { east = None, north = Placed, south = None, west = Optional }
-- { east = None, north = None, south = None, west = Placed }


suite : Test
suite =
    describe "Walls Matches"
        [ test "walls should match"
            (\_ ->
                let
                    w1 =
                        Walls.Walls Placed None None Optional

                    w2 =
                        Walls.Walls Placed None None None
                in
                Expect.equal True (Walls.matches w1 w2)
            )
        , test "match Placed == Placed"
            (\_ -> Expect.equal True (Walls.matchSide (w Placed) .west (w Placed)))
        , test "match Placed == Optional"
            (\_ -> Expect.equal True (Walls.matchSide (w Placed) .west (w Optional)))
        , test "match Placed != None"
            (\_ -> Expect.equal False (Walls.matchSide (w Placed) .west (w None)))
        , test "match Optional == Optional"
            (\_ -> Expect.equal True (Walls.matchSide (w Optional) .west (w Optional)))
        , test "rotate None == None"
            (\_ -> Expect.equal True (Walls.matchSide (w None) .west (w None)))
        , test "rotate clockwise"
            (\_ -> Expect.equal (Walls.Walls None Placed Optional None) (Walls.rotateClockwise (Walls.Walls Placed Optional None None)))

        ]

w west =
    Walls.Walls None None None west
