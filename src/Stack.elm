module Stack exposing (..)


type alias Stack a =
    List a


push : a -> Stack a -> Stack a
push a stack =
    a :: stack


top : Stack a -> Maybe a
top stack =
    List.head stack


popWithReturn : Stack a -> ( Maybe a, Stack a )
popWithReturn stack =
    ( top stack, pop stack )


pop : Stack a -> Stack a
pop stack =
    List.drop 1 stack


contains : a -> Stack a -> Bool
contains a stack =
    stack
        |> List.filter ((==) a)
        |> List.length
        |> (>) 0


topIs: a -> Stack a -> Bool
topIs a stack =
    stack
        |> top
        |> (==) (Just a)
