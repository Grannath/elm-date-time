port module Main exposing (..)

import Json.Encode exposing (Value)
import Test exposing (Test, describe)
import Test.Runner.Node exposing (TestProgram, run)
import TestGregorian
import TestMathExt


main : TestProgram
main =
    run emit all


all : Test
all =
    describe "elm-date-time"
        [ describe "Date.Date"
            [ TestGregorian.all ]
        , TestMathExt.all
        ]


port emit : ( String, Value ) -> Cmd msg
