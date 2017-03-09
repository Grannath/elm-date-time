module TestUtils exposing (..)

import Test exposing (..)
import Expect exposing (..)


testAll : List a -> String -> (a -> Expectation) -> Test
testAll list mes exp =
    Test.concat <|
        List.map (test mes << always << exp) list
