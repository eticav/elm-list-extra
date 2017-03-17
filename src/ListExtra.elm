module ListExtra exposing (..)

lift2 : (a -> b -> c) -> List a -> List b -> List c
lift2 f la lb =
  la
    |> List.concatMap (\a -> lb
               |> List.concatMap (\b -> [f a b]))

lift3 : (a -> b -> c -> d) -> List a -> List b -> List c -> List d
lift3 f la lb lc =
  la
    |> List.concatMap (\a -> lb
                      |> List.concatMap (\b -> lc
                                        |> List.concatMap (\c -> [f a b c])))
        
sequenceCompare : List comparable->List comparable-> Order
sequenceCompare listA listB =
  case (listA,listB) of
    ([],[])->EQ
    ([],b::tailB)->LT
    (a::tailA,[])->GT
    (a::[],b::tailB)->compare a b
    (a::tailA,b::tailB)->
      case compare a b of
        EQ->sequenceCompare tailA tailB
        any->any
