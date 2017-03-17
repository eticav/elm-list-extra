module ListExtraTests exposing (..)

import Expect
import Fuzz
import Test exposing (..)
import ListExtra

tests : Test
tests =
  describe "List extras"
    [ sequenceCompareTest
    ]

sequenceCompareCheck : List Int->List Int->Order->Test
sequenceCompareCheck sub list expected =
  let
    description = (toString sub) ++ " " ++ (toString list) ++ " -> " ++ (toString expected)
  in
  test description (\_->Expect.equal (ListExtra.sequenceCompare sub list) expected)

sequenceCompareTest : Test
sequenceCompareTest =
  describe "sequenceCompare tests"
  [
   sequenceCompareCheck [] [] EQ
  , sequenceCompareCheck [1] [] GT
  , sequenceCompareCheck [] [1] LT                     
  , sequenceCompareCheck [1] [1] EQ
  , sequenceCompareCheck [1] [2] LT
  , sequenceCompareCheck [2] [1] GT
  , sequenceCompareCheck [1,1,1] [1,1,1] EQ
  , sequenceCompareCheck [1,1,1] [1,1,2] LT
  , sequenceCompareCheck [1,1,2] [1,1,1] GT
  , sequenceCompareCheck [1,1,1] [1,2,1] LT
  , sequenceCompareCheck [1,2,1] [1,1,1] GT
  , sequenceCompareCheck [1,1] [1,1,1] EQ
  , sequenceCompareCheck [1,1] [1,1,2] EQ
  , sequenceCompareCheck [1,2] [1,1,2] GT
  , sequenceCompareCheck [1,1] [1,2,2] LT
  , sequenceCompareCheck [1,1,1] [1,1] GT
  , sequenceCompareCheck [1,2,1] [1,1] GT
  , sequenceCompareCheck [1,1,1] [1,2] LT                     
  ]



