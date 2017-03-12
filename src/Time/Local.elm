module Time.Local
    exposing
        ( Data
        , isValid
        , hour
        , minute
        , second
        , milli
        , addHours
        , addMinutes
        , addSeconds
        , addMillis
        , milliOfDay
        , Period
        , period
        , add
        , millisBetween
        , isBefore
        , isAfter
        )

import MathExt exposing (addModulo, addModuloF)
import Tuple


type alias Data =
    { hour : Int
    , minute : Int
    , second : Int
    , milli : Float
    }


hour : Data -> Int
hour =
    .hour


minute : Data -> Int
minute =
    .minute


second : Data -> Int
second =
    .second


milli : Data -> Float
milli =
    .milli


inRange : comparable -> comparable -> comparable -> Bool
inRange from to val =
    from <= val && val < to


isValid : Data -> Bool
isValid data =
    (inRange 0 24 data.hour)
        && (inRange 0 60 data.minute)
        && (inRange 0 61 data.second)
        && (inRange 0 1000 data.milli)


addHours : Int -> Data -> ( Int, Data )
addHours hours data =
    let
        ( days, hour ) =
            addModulo 24 hours data.hour
    in
        ( days
        , { data | hour = hour }
        )


addMinutes : Int -> Data -> ( Int, Data )
addMinutes minutes data =
    let
        ( hours, minute ) =
            addModulo 60 minutes data.minute
    in
        { data | minute = minute }
            |> addHours hours


addSeconds : Int -> Data -> ( Int, Data )
addSeconds seconds data =
    let
        ( minutes, second ) =
            addModulo 60 seconds data.second
    in
        { data | second = second }
            |> addMinutes minutes


addMillis : Float -> Data -> ( Int, Data )
addMillis millis data =
    let
        ( seconds, milli ) =
            addModuloF 1000 millis data.milli
    in
        { data | milli = milli }
            |> addSeconds seconds


milliOfDay : Data -> Float
milliOfDay { hour, minute, second, milli } =
    (hour * 3600000)
        + (minute * 60000)
        + (second * 1000)
        |> toFloat
        |> (+) milli


type alias Period =
    { hours : Int
    , minutes : Int
    , seconds : Int
    , millis : Float
    }


period : Data -> Data -> Period
period data1 data2 =
    let
        dh =
            data2.hour - data1.hour

        hours =
            if isAfter (addHours dh data1 |> Tuple.second) data2 then
                dh - 1
            else
                dh

        ch =
            addHours hours data1 |> Tuple.second

        dm =
            data2.minute - data1.minute

        minutes =
            if isAfter (addMinutes dm ch |> Tuple.second) data2 then
                dm - 1
            else
                dm

        cm =
            addMinutes minutes ch |> Tuple.second

        ds =
            data2.second - data1.second

        seconds =
            if isAfter (addSeconds ds cm |> Tuple.second) data2 then
                ds - 1
            else
                ds

        millis =
            millisBetween (addSeconds seconds cm |> Tuple.second) data2
    in
        { hours = hours
        , minutes = minutes
        , seconds = seconds
        , millis = millis
        }


add : Period -> Data -> ( Int, Data )
add prd data =
    let
        addMap fn ( days, data ) =
            let
                ( days2, nData ) =
                    fn data
            in
                ( days + days2
                , nData
                )
    in
        addMillis prd.millis data
            |> addMap (addSeconds prd.seconds)
            |> addMap (addMinutes prd.minutes)
            |> addMap (addHours prd.hours)


millisBetween : Data -> Data -> Float
millisBetween data1 data2 =
    (data2.hour - data1.hour)
        * 3600000
        + (data2.minute - data1.minute)
        * 60000
        + (data2.second - data1.second)
        * 1000
        |> toFloat
        |> (+) (data2.milli - data1.milli)


isBefore : Data -> Data -> Bool
isBefore data1 data2 =
    (data1.hour < data2.hour)
        || (data1.hour == data2.hour && data1.minute < data2.minute)
        || (data1.hour == data2.hour && data1.minute == data2.minute && data1.second < data2.second)
        || (data1.hour == data2.hour && data1.minute == data2.minute && data1.second == data2.second && data1.milli < data2.milli)


isAfter : Data -> Data -> Bool
isAfter data1 data2 =
    (data1.hour > data2.hour)
        || (data1.hour == data2.hour && data1.minute > data2.minute)
        || (data1.hour == data2.hour && data1.minute == data2.minute && data1.second > data2.second)
        || (data1.hour == data2.hour && data1.minute == data2.minute && data1.second == data2.second && data1.milli > data2.milli)
