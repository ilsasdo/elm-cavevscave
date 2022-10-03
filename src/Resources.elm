module Resources exposing (..)

import Walls exposing (Wall(..), Walls)


type alias Resources =
    { food : Int
    , wood : Int
    , stone : Int
    , emmer : Int
    , flax : Int
    , gold : Int
    }


noWalls : Walls
noWalls =
    Walls Optional Optional Optional Optional


priceWood : Int -> Resources -> Resources
priceWood qty resources =
    { resources | wood = resources.wood + qty }


priceFood : Int -> Resources -> Resources
priceFood qty resources =
    { resources | food = resources.food + qty }


priceStone : Int -> Resources -> Resources
priceStone qty resources =
    { resources | stone = resources.stone + qty }


priceGold : Int -> Resources -> Resources
priceGold qty resources =
    { resources | gold = resources.gold + qty }


priceFree : Resources
priceFree =
    Resources 0 0 0 0 0 0
