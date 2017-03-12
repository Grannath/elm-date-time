module Time.Time
    exposing
        ( Time(..)
        , TimeOps
        , CompOps
        , Local
        , hour
        , minute
        , second
        , milli
        , milliOfDay
        , addHours
        , addMinutes
        , addSeconds
        , addMillis
        , addHoursO
        , addMinutesO
        , addSecondsO
        , addMillisO
        , localTime
        , toLocal
        , Period
        , period
        , add
        , addO
        , millisBetween
        , isBefore
        , isAfter
        )

{-| This module contains the Time type and relating functions. It is used to
work with pure time values without an attached date.

The is both support for a local time, which has no offset, or an offset time.
The later has a simple offset from UTC, but no concrete time zone. Without a
date, it is impossible to determine the offset from a zone.

# Time
@docs Time, hour, minute, second, milli, milliOfDay, addHours, addMinutes, addSeconds, addMillis, addHoursO, addMinutesO, addSecondsO, addMillisO

# Local time
@docs Local, localTime, toLocal

# Comparisons
@docs Period, period, add, addO, millisBetween, isBefore, isAfter

# Extension
@docs TimeOps, CompOps
-}

import Time.Local as LocalTime
import Tuple exposing (second, mapSecond)


{-| Time represents a time as shown on a wall clock. It does not hold
information about a date.

Time can represent values with or without an offset from UTC. It cannot
represent values with a time zone, as the calculation of an offset from a time
zone needs a date.
-}
type Time a
    = Time { data : a, ops : TimeOps a }


{-| TimeOps holds all necessary operations on a Time type.
-}
type alias TimeOps a =
    { hour : a -> Int
    , minute : a -> Int
    , second : a -> Int
    , milli : a -> Float
    , milliOfDay : a -> Float
    , addHours : Int -> a -> ( Int, a )
    , addMinutes : Int -> a -> ( Int, a )
    , addSeconds : Int -> a -> ( Int, a )
    , addMillis : Float -> a -> ( Int, a )
    , toLocal : a -> Time Local
    , compOps : CompOps a
    }


{-| CompOps contains all functions for comparisons between Times.
-}
type alias CompOps data =
    { period : data -> data -> Period
    , add : Period -> data -> ( Int, data )
    , millisBetween : data -> data -> Float
    , isBefore : data -> data -> Bool
    , isAfter : data -> data -> Bool
    }


{-| Period represents a difference between two Times.
-}
type Period
    = Period
        { hours : Int
        , minutes : Int
        , seconds : Int
        , millis : Float
        }


{-| Local represents a local time without any information about an offset.
-}
type Local
    = Local LocalTime.Data


wrapArg : (LocalTime.Data -> a) -> Local -> a
wrapArg f =
    (\(Local d) ->
        f d
    )


wrapArgs2 :
    (a -> LocalTime.Data -> b)
    -> a
    -> Local
    -> b
wrapArgs2 func =
    (\d -> wrapArg (func d))


wrapArgs1 :
    (LocalTime.Data -> a -> b)
    -> Local
    -> a
    -> b
wrapArgs1 func =
    (\d x -> wrapArg (flip func x) d)


(<<<) : (c -> d) -> (a -> b -> c) -> a -> b -> d
(<<<) f2 f1 =
    (\x y ->
        f1 x y |> f2
    )


(>>>) : (a -> b -> c) -> (c -> d) -> a -> b -> d
(>>>) f1 f2 =
    (\x y ->
        f1 x y |> f2
    )


wrapRes : ( a, LocalTime.Data ) -> ( a, Local )
wrapRes ( val, data ) =
    ( val, Local data )


localOps : TimeOps Local
localOps =
    { hour = wrapArg LocalTime.hour
    , minute = wrapArg LocalTime.minute
    , second = wrapArg LocalTime.second
    , milli = wrapArg LocalTime.milli
    , addHours = wrapRes <<< wrapArgs2 LocalTime.addHours
    , addMinutes = wrapRes <<< wrapArgs2 LocalTime.addMinutes
    , addSeconds = wrapRes <<< wrapArgs2 LocalTime.addSeconds
    , addMillis = wrapRes <<< wrapArgs2 LocalTime.addMillis
    , milliOfDay = wrapArg LocalTime.milliOfDay
    , toLocal = \d -> Time { data = d, ops = localOps }
    , compOps = localComps
    }


localComps : CompOps Local
localComps =
    { period = Period <<< (wrapArgs1 <| wrapArgs2 LocalTime.period)
    , add = localAddPeriod
    , millisBetween = wrapArgs1 <| wrapArgs2 LocalTime.millisBetween
    , isBefore = wrapArgs1 <| wrapArgs2 LocalTime.isBefore
    , isAfter = wrapArgs1 <| wrapArgs2 LocalTime.isAfter
    }


localAddPeriod : Period -> Local -> ( Int, Local )
localAddPeriod (Period prd) (Local data) =
    LocalTime.add prd data
        |> mapSecond Local


{-| hour returns the hour of the given Time.
-}
hour : Time a -> Int
hour (Time { data, ops }) =
    ops.hour data


{-| minute returns the minute of the given Time.
-}
minute : Time a -> Int
minute (Time { data, ops }) =
    ops.minute data


{-| second returns the second of the given Time.
-}
second : Time a -> Int
second (Time { data, ops }) =
    ops.second data


{-| milli returns the milliseconds of the given Time.
-}
milli : Time a -> Float
milli (Time { data, ops }) =
    ops.milli data


{-| milliOfDay returns the milliseconds since midnight of the given Time.
-}
milliOfDay : Time a -> Float
milliOfDay (Time { data, ops }) =
    ops.milliOfDay data


asTime : TimeOps a -> a -> Time a
asTime ops data =
    Time { data = data, ops = ops }


{-| addHoursO adds a number of hours to a Time, returning the new Time and the
number of times the value has overflowed.

For example,
    localTime 23 30 0 0 |> addHoursO 1
    => ( 1, ... )
-}
addHoursO : Int -> Time a -> ( Int, Time a )
addHoursO hours (Time { data, ops }) =
    ops.addHours hours data
        |> mapSecond (asTime ops)


{-| addHours adds a number of hours to a Time and returns the new Time.
-}
addHours : Int -> Time a -> Time a
addHours =
    addHoursO >>> Tuple.second


{-| addMinutesO adds minutes to a Time and returns the new Time and the number
of overflows.
-}
addMinutesO : Int -> Time a -> ( Int, Time a )
addMinutesO minutes (Time { data, ops }) =
    ops.addMinutes minutes data
        |> mapSecond (asTime ops)


{-| addMinutes adds a number of minutes to a Time and returns the new Time.
-}
addMinutes : Int -> Time a -> Time a
addMinutes =
    addMinutesO >>> Tuple.second


{-| addSecondsO adds seconds to a Time and returns the new Time and the number
of overflows.
-}
addSecondsO : Int -> Time a -> ( Int, Time a )
addSecondsO seconds (Time { data, ops }) =
    ops.addSeconds seconds data
        |> mapSecond (asTime ops)


{-| addSeconds adds a number of seconds to a Time and returns the new Time.
-}
addSeconds : Int -> Time a -> Time a
addSeconds =
    addSecondsO >>> Tuple.second


{-| addMillisO adds milliseconds to a Time and returns the new Time and the
number of overflows.
-}
addMillisO : Float -> Time a -> ( Int, Time a )
addMillisO millis (Time { data, ops }) =
    ops.addMillis millis data
        |> mapSecond (asTime ops)


{-| addMillis adds a number of milliseconds to a Time and returns the new Time.
-}
addMillis : Float -> Time a -> Time a
addMillis =
    addMillisO >>> Tuple.second


{-| localTime constructs a new Time from hour, minute, second and milliseconds,
without any offset.
-}
localTime : Int -> Int -> Int -> Float -> Result String (Time Local)
localTime hour minute second milli =
    let
        data =
            { hour = hour
            , minute = minute
            , second = second
            , milli = milli
            }
    in
        if LocalTime.isValid data then
            asTime localOps (Local data)
                |> Result.Ok
        else
            Result.Err
                ((toString hour)
                    ++ ":"
                    ++ (toString minute)
                    ++ ":"
                    ++ (toString second)
                    ++ ":"
                    ++ (toString milli)
                    ++ " is not a valid local time."
                )


{-| toLocal transforms any Time into a local value without offset. It should
have the same values for hour, minute, second and milli as the original.
-}
toLocal : Time a -> Time Local
toLocal (Time { data, ops }) =
    ops.toLocal data


{-| period gives the difference between two Times. The values are all
normalized.
-}
period : Time a -> Time a -> Period
period (Time time1) (Time time2) =
    time1.ops.compOps.period time1.data time2.data


{-| addO adds a Period to a Time, giving both the new Time as well as the number
of overflows.
-}
addO : Period -> Time a -> ( Int, Time a )
addO period (Time { data, ops }) =
    ops.compOps.add period data
        |> mapSecond (asTime ops)


{-| add adds a Period to a Time.
-}
add : Period -> Time a -> Time a
add =
    addO >>> Tuple.second


{-| millisBetween gives the number of milliseconds between two Times.
-}
millisBetween : Time a -> Time a -> Float
millisBetween (Time time1) (Time time2) =
    time1.ops.compOps.millisBetween
        time1.data
        time2.data


{-| isBefore returns True is the first Time is before the second.
-}
isBefore : Time a -> Time a -> Bool
isBefore (Time time1) (Time time2) =
    time1.ops.compOps.isBefore
        time1.data
        time2.data


{-| isAfter returns True if the first Time is after the second.
-}
isAfter : Time a -> Time a -> Bool
isAfter (Time time1) (Time time2) =
    time1.ops.compOps.isAfter
        time1.data
        time2.data
