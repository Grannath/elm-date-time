port module Main exposing (..)

import Json.Encode exposing (Value)
import Test exposing (Test, describe)
import Test.Runner.Node exposing (TestProgram, run)
import TestDate


main : TestProgram
main =
    run emit all


all : Test
all =
    describe "elm-utc"
        [ TestDate.all ]


port emit : ( String, Value ) -> Cmd msg
