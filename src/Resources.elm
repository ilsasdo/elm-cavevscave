module Resources exposing (..)

import Game exposing (Resources, Wall(..), Walls)


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
    Resources 0 0 0 0 0 0 0 7 -1


updateOpponentsGold : Int -> Resources -> Resources
updateOpponentsGold qty resources =
    { resources | opponentsGold = qty }


atLeastThreeResources : Resources -> Bool
atLeastThreeResources resources =
    [resources.wood, resources.emmer, resources.gold, resources.food, resources.flax, resources.stone]
    |> List.filter (\qty -> qty > 0)
    |> List.length
    |> (<=) 3
