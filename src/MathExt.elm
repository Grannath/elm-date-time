module MathExt
    exposing
        ( (///)
        , (%%%)
        , remf
        , addModulo
        , addModuloF
        )



(///) : Float -> Float -> Int
(///) numb mod =
    let
        div =
            (numb / mod)
    in
        if div > 0 then floor div else ceiling div


(%%%) : Float -> Float -> Float
(%%%) numb mod =
    let
        sign =
            if mod < 0 then
                -1
            else
                1

        abMod =
            sign * mod

        md =
            remf numb mod
    in
        if md < 0 then
            sign * (md + abMod)
        else
            sign * md


remf : Float -> Float -> Float
remf numb mod =
    numb - mod * (toFloat <| numb /// mod)


addModulo : Int -> Int -> Int -> ( Int, Int )
addModulo mod amnt val =
    let
        res =
            (val + amnt) % mod

        offset =
            if val + amnt < 0 then
                -1
            else
                0

        overflow =
            ((val + amnt - offset) // mod) + offset
    in
        ( overflow, res )


addModuloF : Float -> Float -> Float -> ( Int, Float )
addModuloF mod amnt val =
    let
        res =
            (val + amnt) %%% mod

        offset =
            if val + amnt < 0 then
                -1
            else
                0

        overflow =
            ((val + amnt - offset) /// mod) + offset
    in
        ( overflow, res )
