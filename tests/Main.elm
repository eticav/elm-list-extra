module Main exposing (..)

import Test exposing (..)
import Test.Runner.Html

import ListExtraTests

main : Test.Runner.Html.TestProgram
main =
  [ ListExtraTests.tests
  ]
     |> concat
     |> Test.Runner.Html.run        
