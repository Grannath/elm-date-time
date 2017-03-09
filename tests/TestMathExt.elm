module TestMathExt exposing (all)

import TestUtils exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (int, intRange, float, floatRange)
import MathExt exposing (..)
import Test exposing (..)


all : Test
all =
    describe "MathExt"
        [ floorDiv
        , moduloFloat
        , remainderFloat
        , equivalences
        ]


floorDiv : Test
floorDiv =
    describe "floor division (///)"
        [ fuzz2 float float "has the correct sign" <|
            \a b ->
                let
                    div =
                        a /// b
                in
                    Expect.true
                        ((toString a)
                            ++ "///"
                            ++ (toString b)
                            ++ " does not have sign "
                            ++ (toString <| (signf a) * (signf b))
                            ++ " but "
                            ++ (toString <| sign div)
                        )
                        (div == 0 || (sign div) == (signf a) * (signf b))
        , fuzz2 float float "works independently of sign" <|
            \a b ->
                Expect.true
                    ((toString a)
                        ++ "///"
                        ++ (toString b)
                        ++ " should be the same as -("
                        ++ (toString a)
                        ++ "///"
                        ++ (toString -b)
                        ++ ")"
                    )
                    (b == 0 || a /// b == -(a /// -b))
        ]


modulos : List ( Float, Float, Float )
modulos =
    [ ( 12.25, 4.5, 3.25 )
    , ( -12.25, 4.5, 1.25 )
    , ( 1.37, -2.0, -1.37 )
    , ( 7.5, 3.75, 0 )
    , ( 3.47, 12.456, 3.47 )
    ]


moduloFloat : Test
moduloFloat =
    describe "float modulus (%%%)"
        [ fuzz2 float float "has the correct sign" <|
            \a b ->
                let
                    md =
                        a %%% b
                in
                    Expect.true
                        ((toString a)
                            ++ "%%%"
                            ++ (toString b)
                            ++ " does not have sign "
                            ++ (toString <| signf b)
                            ++ " but "
                            ++ (toString <| signf md)
                        )
                        (b == 0 || md == 0 || (signf md) == (signf b))
        , testAll modulos "gives the expected result" <|
            \( a, b, md ) ->
                let
                    rs =
                        a %%% b
                in
                    Expect.true
                        ((toString a)
                            ++ "%%%"
                            ++ (toString b)
                            ++ " should be "
                            ++ (toString md)
                            ++ " but is "
                            ++ (toString rs)
                        )
                        (md == rs)
        ]


remainderFloat : Test
remainderFloat =
    describe "float remainder remf"
        [ fuzz2 float float "has the correct sign" <|
            \a b ->
                let
                    rm =
                        remf a b
                in
                    Expect.true
                        ("rem "
                            ++ (toString a)
                            ++ " "
                            ++ (toString b)
                            ++ " does not have sign "
                            ++ (toString <| signf a)
                            ++ " but "
                            ++ (toString <| signf rm)
                        )
                        (b == 0 || a == 0 || rm == 0 || (signf rm) == (signf a))
        ]


equivalences : Test
equivalences =
    describe "hold all equivalences"
        [ fuzz2 (floatRange 0.1 1.0e10) (floatRange 0.1 1.0e10) "remf a b == a %%% b for a, b > 0" <|
            \a b ->
                let
                    rm =
                        remf a b

                    md =
                        a %%% b
                in
                    Expect.true
                        ("For a = "
                            ++ (toString a)
                            ++ " and b = "
                            ++ (toString b)
                            ++ " is remf a b == "
                            ++ (toString rm)
                            ++ ", but a %%% b == "
                            ++ (toString md)
                        )
                        (md == rm)
        ]



-- Utils


sign : Int -> Int
sign a =
    if a < 0 then
        -1
    else
        1


signf : Float -> Int
signf a =
    if a < 0 then
        -1
    else
        1
