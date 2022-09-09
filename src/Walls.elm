module Walls exposing (..)

type Wall
    = Fixed
    | Placed
    | Required
    | Optional
    | None

type alias Walls =
    { north : Wall
    , east : Wall
    , south : Wall
    , west : Wall
    }