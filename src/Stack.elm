module Stack exposing (..)


type alias Stack a =
    List a


push : a -> Stack a -> Stack a
push a stack =
    a :: stack


top : Stack a -> Maybe a
top stack =
    List.head stack


pop : Stack a -> ( Maybe a, Stack a )
pop stack =
    ( top stack, List.drop 1 stack )


contains : a -> Stack a -> Bool
contains a stack =
    stack
        |> List.filter ((==) a)
        |> List.length
        |> (>) 0
