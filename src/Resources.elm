module Resources exposing (..)

import Game exposing (Resources, Wall(..), Walls)


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

require : (Resources -> Int) -> (Int -> Int -> Bool) -> Int -> Resources -> Bool
require getter condition qty resources =
    condition (getter resources) qty


topWood : Int -> Resources -> Resources
topWood qty resources =
    { resources | wood = Basics.max resources.wood qty }


topFood : Int -> Resources -> Resources
topFood qty resources =
    { resources | food = Basics.max resources.food qty }


topStone : Int -> Resources -> Resources
topStone qty resources =
    { resources | stone = Basics.max resources.stone qty }


topGold : Int -> Resources -> Resources
topGold qty resources =
    { resources | gold = Basics.max resources.gold qty }


minStone : Int -> Resources -> Resources
minStone qty resources =
    { resources | stone = Basics.min resources.stone qty }


topFlax : Int -> Resources -> Resources
topFlax qty resources =
    { resources | flax = Basics.max resources.flax qty }


topEmmer : Int -> Resources -> Resources
topEmmer qty resources =
    { resources | emmer = Basics.max resources.emmer qty }


addWood : Int -> Resources -> Resources
addWood qty resources =
    { resources | wood = min 9 (resources.wood + qty) }


addStone : Int -> Resources -> Resources
addStone qty resources =
    { resources | stone = min 9 (resources.stone + qty) }


addFlax : Int -> Resources -> Resources
addFlax qty resources =
    { resources | flax = min 9 (resources.flax + qty) }


addEmmer : Int -> Resources -> Resources
addEmmer qty resources =
    { resources | emmer = min 9 (resources.emmer + qty) }


addFood : Int -> Resources -> Resources
addFood qty resources =
    { resources | food = min 9 (resources.food + qty) }


addGold : Int -> Resources -> Resources
addGold qty resources =
    { resources | gold = min 19 (resources.gold + qty) }


alwaysDoable : Resources -> Bool
alwaysDoable resources =
    True
