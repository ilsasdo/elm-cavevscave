module Walls exposing (..)


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
