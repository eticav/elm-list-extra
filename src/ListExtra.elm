module ListExtra exposing (..)

import Dict exposing (..)
import Set

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


updateAt : Int-> (a -> a) -> List a -> List a
updateAt index fun list =
  case index > 0 of
    True->
      let
        h = List.take (index-1) list
        t = List.drop (index-1) list
        t2 = List.drop 1 t
        element = case (List.head t) of
                    Just e -> [fun e]
                    _ -> []
      in
        List.append h (List.append element t2)
    _-> list

nth : Int -> List a -> Maybe a
nth index list =
  case index > 0 of
    True->
      let
        t = List.drop (index-1) list
      in
        List.head t
    _-> Nothing
    
removeAt : Int->  List a -> List a
removeAt index list =
  case index > 0 of
    True->
      let
        h = List.take (index-1) list
        t = List.drop (index) list
      in
        List.append h t
    _-> list
    
applyAccumulate : ((a,c) -> (b,c)) -> a -> (List b,c) -> (List b,c)
applyAccumulate f item (acclist,acc) =
  let
    (newItem,newAcc)=f (item,acc)
  in  
   ( newItem :: acclist ,newAcc)
               
mapfoldr : ((a,c) -> (b,c)) -> c -> List a -> (List b,c)
mapfoldr f acc list =
  let
     (newList, newAcc)=List.foldr (applyAccumulate f) ([],acc) list
  in
    (List.reverse newList, newAcc)

mapfoldl : ((a,c) -> (b,c)) -> c -> List a -> (List b,c)
mapfoldl f acc list =
  let
     (newList, newAcc)=List.foldl (applyAccumulate f) ([],acc) list
  in
    (List.reverse newList, newAcc)

mapIndexl : ((a,Int) -> b) -> Int -> Int-> List a -> List b
mapIndexl f acc inc list =
  let
    (newList, _)=mapfoldl (\(item,index)-> (f (item,index), index+inc )) acc list
  in
    newList

mapIndexr : ((a,Int) -> b) -> Int -> Int-> List a -> List b
mapIndexr f acc inc list =
  let
    (newList, _)=mapfoldr (\(item,index)-> (f (item,index), index+inc )) acc list
  in
    newList

at : (a->Bool) -> List a -> Maybe a
at f list =
    case list of
      x :: xs ->
        case f x of
          True -> Just x
          False -> at f xs
      [] -> Nothing

removeWhenFirst : (a->Bool) -> List a -> List a
removeWhenFirst filter list =
  case list of
    x :: xs ->
      case filter x of
        True -> xs
        _ -> x :: removeWhenFirst filter xs
    [] -> []
    
updateFirst : (Maybe a->Maybe a)-> List a -> List a
updateFirst update list =
  case list of
    x :: xs ->
      case update (Just x) of
        Just res -> res :: xs
        _ -> x :: updateFirst update xs
    [] ->
      case update Nothing of
        Just res -> [res]
        _ -> []
        
partition : (a -> comparable) -> List a -> Dict.Dict comparable (List a)
partition f list =
  let
    adder x acc =
      let
        key = f x
        partition = case Dict.get key acc of
                      Just p -> x::p
                      Nothing -> [x]
      in
        Dict.insert key partition acc
  in  
    List.foldl adder Dict.empty list

uniqueBy : (a -> comparable) -> List a -> List a
uniqueBy f list =
    uniqueHelp f Set.empty list
        
uniqueHelp : (a -> comparable) -> Set.Set comparable -> List a -> List a
uniqueHelp f existing remaining =
  case remaining of
    [] -> []
    first :: rest ->
      let
        computedFirst =
          f first
      in
        if Set.member computedFirst existing then
          uniqueHelp f existing rest
        else
          first :: uniqueHelp f (Set.insert computedFirst existing) rest
                          
merge : (a -> comparable)->List a->List a->List a
merge f list1 list2 =
  List.append list1 list2
    |> uniqueBy f
