module Date.Date
    exposing
        ( Date(..)
        , DateOps
        , WeekOps
        , CompOps
        , Period(..)
        , Weekday(..)
        , Gregorian
        , gregorian
        , isoWeekOps
        , day
        , month
        , year
        , dayOfYear
        , daysInMonth
        , daysInYear
        , addDays
        , addMonths
        , addYears
        , isLeapYear
        , period
        , add
        , daysBetween
        , isBefore
        , isAfter
        , dayOfWeek
        , weekOfYear
        , weekOfWeekBasedYear
        , weekBasedYear
        )

{-| This module defines a Date type to represent a unique day in any calendar system.

Note that there is no ZonedDate module. Attaching a zone to a date makes no
sense, as conversion between zones is impossible without a time. To carry the
information, you can always resort to a pair or a ZonedDateTime. The same
applies to offsets.

# Dates
@docs Date, Weekday, day, month, year, dayOfYear, daysInMonth, daysInYear, addDays, addMonths, addYears, isLeapYear

# Weeks
@docs dayOfWeek, weekOfYear, weekOfWeekBasedYear, weekBasedYear

# Comparisons
@docs Period, period, add, daysBetween, isBefore, isAfter

# Gregorian calendar
@docs gregorian

# Extension
@docs DateOps, WeekOps, CompOps, Gregorian, isoWeekOps
-}

import Date.Gregorian as GregorianDate


{-| Date represents a unique day in any calendar system.

A Date consists of two parts. One holds the data used by a given calendar
system, the other holds functions to read and manipulate the data. By providing
both, any calendar system can be supported.
-}
type Date data
    = Date { data : data, ops : DateOps data }


{-| DateOps defines the minimum operations necessary to implement a calendar system.

Some operations are optional, such as isLeapYear. These do not have to be
supplied to define a working calendar system, but are common enough to be listed
here. Some systems may have different variants of some operations. Different
countries, for example, use different first days of the week, although all other
rules follow the gregorian calendar.
-}
type alias DateOps data =
    { day : data -> Int
    , month : data -> Int
    , year : data -> Int
    , dayOfYear : data -> Int
    , addDays : Int -> data -> data
    , addMonths : Int -> data -> data
    , addYears : Int -> data -> data
    , weekOps : Maybe (WeekOps data)
    , compOps : CompOps data
    , daysInMonth : data -> Int
    , daysInYear : data -> Int
    , isLeapYear : Maybe (data -> Bool)
    , toGregorian : data -> Date Gregorian
    }


{-| WeekOps defines special operations and standards regarding weeks and weekdays.

Using these definitions, values like dayOfWeek or weekOfYear can be calculated.
Not all calendar systems support this, while others might provide multiple
variants.
-}
type alias WeekOps data =
    { dayOfWeek : data -> Weekday
    , firstDayOfWeek : Weekday
    , minimalDaysInFirstWeek : Int
    }


{-| CompOps contains all functions for comparisons between Dates.
-}
type alias CompOps data =
    { period : data -> data -> Period
    , add : Period -> data -> data
    , daysBetween : data -> data -> Int
    , isBefore : data -> data -> Bool
    , isAfter : data -> data -> Bool
    }


{-| Period describes a length of time as found on a calendar.

Keep in mind that without a reference a Period is ambigious. A Period of one
month for example can have a different length in days, depending on the starting
date. For absolute differences, use daysBetween.

As an example, consider 2000-01-31 and 2000-02-29. Both are the last day of
their respective month, so the period from the first to the second will be
counted as one month. However, the period from the second to the first will be
29 days, as one month would lead to 2000-01-29.
-}
type Period
    = Period
        { days : Int
        , months : Int
        , years : Int
        }


{-| Data type used to represent the days of the week.
-}
type Weekday
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun



-- Gregorian calendar


{-| Gregorian represents a day in the proleptic gregorian calendar.
-}
type Gregorian
    = Gregorian GregorianDate.Data


{-| gregorian tries to construct a Date in the proleptic gregorian calendar from
year, month and date.
-}
gregorian : Int -> Int -> Int -> Result String (Date Gregorian)
gregorian year month day =
    let
        data =
            { year = year
            , month = month
            , day = day
            }
    in
        if GregorianDate.isValidDate data then
            Ok (Date { data = Gregorian data, ops = gregorianOps })
        else
            Err
                ((toString year)
                    ++ "-"
                    ++ (toString month)
                    ++ "-"
                    ++ (toString day)
                    ++ " is not a valid gregorian date."
                )


wrapArg : (GregorianDate.Data -> a) -> Gregorian -> a
wrapArg f =
    (\(Gregorian d) ->
        f d
    )


wrapArgs2 :
    (a -> GregorianDate.Data -> b)
    -> a
    -> Gregorian
    -> b
wrapArgs2 func =
    (\d -> wrapArg (func d))


wrapArgs1 :
    (GregorianDate.Data -> a -> b)
    -> Gregorian
    -> a
    -> b
wrapArgs1 func =
    (\d x -> wrapArg (flip func x) d)


(<<<) : (c -> d) -> (a -> b -> c) -> a -> b -> d
(<<<) f2 f1 =
    (\x y ->
        f1 x y |> f2
    )


gregorianOps : DateOps Gregorian
gregorianOps =
    { day = wrapArg GregorianDate.day
    , month = wrapArg GregorianDate.month
    , year = wrapArg GregorianDate.year
    , dayOfYear = wrapArg GregorianDate.dayOfYear
    , addDays = Gregorian <<< wrapArgs2 GregorianDate.addDays
    , addMonths = Gregorian <<< wrapArgs2 GregorianDate.addMonths
    , addYears = Gregorian <<< wrapArgs2 GregorianDate.addYears
    , isLeapYear = Just (wrapArg (GregorianDate.isLeapYear << GregorianDate.year))
    , daysInMonth = wrapArg GregorianDate.daysInMonth
    , daysInYear = wrapArg GregorianDate.daysInYear
    , weekOps = Just isoWeekOps
    , compOps = gregorianCompOps
    , toGregorian = (\d -> Date { data = d, ops = gregorianOps })
    }


gregorianCompOps : CompOps Gregorian
gregorianCompOps =
    { period = gregorianPeriod
    , add = gregorianAddPeriod
    , daysBetween = wrapArgs1 <| wrapArgs2 GregorianDate.daysBetween
    , isBefore = wrapArgs1 <| wrapArgs2 GregorianDate.isBefore
    , isAfter = wrapArgs1 <| wrapArgs2 GregorianDate.isAfter
    }


{-| isoWeekOps holds the definition from ISO-8601 setting the first day of a
week to monday and the minimum number of days in a week to 4.
-}
isoWeekOps : WeekOps Gregorian
isoWeekOps =
    { dayOfWeek = weekday
    , firstDayOfWeek = Mon
    , minimalDaysInFirstWeek = 4
    }


weekday : Gregorian -> Weekday
weekday (Gregorian data) =
    let
        d =
            GregorianDate.weekday data
    in
        if d == 0 then
            Sun
        else if d == 1 then
            Mon
        else if d == 2 then
            Tue
        else if d == 3 then
            Wed
        else if d == 4 then
            Thu
        else if d == 5 then
            Fri
        else
            Sat


gregorianPeriod : Gregorian -> Gregorian -> Period
gregorianPeriod (Gregorian data1) (Gregorian data2) =
    let
        ( years, months, days ) =
            GregorianDate.period data1 data2
    in
        Period
            { years = years
            , months = months
            , days = days
            }


gregorianAddPeriod : Period -> Gregorian -> Gregorian
gregorianAddPeriod (Period { days, months, years }) (Gregorian data) =
    GregorianDate.addPeriod ( years, months, days ) data
        |> Gregorian



-- General methods


split : Date a -> ( a, DateOps a )
split (Date { data, ops }) =
    ( data, ops )


{-| day returns the day as defined by the calendar in use.
-}
day : Date a -> Int
day (Date { data, ops }) =
    ops.day data


{-| month returns the month as defined by the calendar in use.
-}
month : Date a -> Int
month (Date { data, ops }) =
    ops.month data


{-| year returns the year as defined by the calendar in use.
-}
year : Date a -> Int
year (Date { data, ops }) =
    ops.year data


{-| dayOfYear returns the number of the day, counting from the beginning of the
current year.
-}
dayOfYear : Date a -> Int
dayOfYear (Date { data, ops }) =
    ops.dayOfYear data


{-| daysInMonth returns the number of days in the given month.
-}
daysInMonth : Date a -> Int
daysInMonth (Date { data, ops }) =
    ops.daysInMonth data


{-| daysInYear returns the number of days in the given year.
-}
daysInYear : Date a -> Int
daysInYear (Date { data, ops }) =
    ops.daysInYear data


asDate : DateOps a -> a -> Date a
asDate ops data =
    Date { data = data, ops = ops }


{-| addDays adds the given number of days to the date, adjusting month and year
where necessary.
-}
addDays : Int -> Date a -> Date a
addDays days (Date { data, ops }) =
    ops.addDays days data
        |> asDate ops


{-| addMonths adds the given number of months to the date, adjusting the year
where necessary.

Since not all months have the same number of days, the day will be clamped down
to the first valid number.
-}
addMonths : Int -> Date a -> Date a
addMonths months (Date { data, ops }) =
    ops.addMonths months data
        |> asDate ops


{-| addYears adds the given number of years to the date. Other fields are
clamped down when necessary.
-}
addYears : Int -> Date a -> Date a
addYears years (Date { data, ops }) =
    ops.addYears years data
        |> asDate ops


{-| isLeapYear returns True if the calendar system has leap years and the given
date is part of one.
-}
isLeapYear : Date a -> Bool
isLeapYear (Date { data, ops }) =
    ops.isLeapYear
        |> Maybe.map ((|>) data)
        |> Maybe.withDefault False


{-| period returns the relative time between Dates.

Please note that
    period date1 date2 \= -(period date2 date1)
but
    add (period date1 date2) date1 == date2
-}
period : Date a -> Date a -> Period
period date1 date2 =
    let
        ( data1, ops1 ) =
            split date1

        ( data2, ops2 ) =
            split date2
    in
        ops1.compOps.period data1 data2


{-| daysBetween returns the absolute difference between two Dates.
-}
daysBetween : Date a -> Date a -> Int
daysBetween date1 date2 =
    let
        ( data1, ops1 ) =
            split date1

        ( data2, ops2 ) =
            split date2
    in
        ops1.compOps.daysBetween data1 data2


{-| add adds the days, months and years described by the Period to the given
Date.
-}
add : Period -> Date a -> Date a
add period (Date { data, ops }) =
    ops.compOps.add period data
        |> asDate ops


{-| isBefore returns True if the first Date comes before the second.
-}
isBefore : Date a -> Date a -> Bool
isBefore date1 date2 =
    let
        ( data1, ops1 ) =
            split date1

        ( data2, ops2 ) =
            split date2
    in
        ops1.compOps.isBefore data1 data2


{-| isAfter returns True if the first Date comes after the second.
-}
isAfter : Date a -> Date a -> Bool
isAfter date1 date2 =
    let
        ( data1, ops1 ) =
            split date1

        ( data2, ops2 ) =
            split date2
    in
        ops1.compOps.isAfter data1 data2


{-| dayOfWeek returns the Weekday if the calendar system in use supports this
operation.
-}
dayOfWeek : Date a -> Maybe Weekday
dayOfWeek (Date { data, ops }) =
    case ops.weekOps of
        Just wOps ->
            Just (wOps.dayOfWeek data)

        Nothing ->
            Nothing


startOfWeekOffset : DateOps a -> WeekOps a -> a -> Int
startOfWeekOffset ops wOps data =
    let
        dayNumber day =
            case day of
                Mon ->
                    1

                Tue ->
                    2

                Wed ->
                    3

                Thu ->
                    4

                Fri ->
                    5

                Sat ->
                    6

                Sun ->
                    7

        ldn wOps =
            (((dayNumber <| wOps.dayOfWeek data) - (dayNumber <| wOps.firstDayOfWeek)) % 7) + 1

        doy =
            ops.dayOfYear data

        n =
            (doy - (ldn wOps)) % 7
    in
        if n + 1 > wOps.minimalDaysInFirstWeek then
            7 - n
        else
            -n


week : DateOps a -> WeekOps a -> a -> Int
week ops wOps data =
    let
        doy =
            ops.dayOfYear data
    in
        (6 + (startOfWeekOffset ops wOps data) + doy) // 7


{-| weekOfYear returns the number of the week as defined by the calendar system.

This starts on the first week of the year that has a certain minimal number of
days. Days in the overlapping week are counted as in week 52/53 or 0, depending
on the calendar year.
-}
weekOfYear : Date a -> Maybe Int
weekOfYear (Date { data, ops }) =
    case ops.weekOps of
        Just wOps ->
            Just (week ops wOps data)

        Nothing ->
            Nothing


{-| weekOfWeekBasedYear gives the week number based on a year that only has whole weeks.

In this system, every week belongs to exactly one year, and therefore no week 0
exists.
-}
weekOfWeekBasedYear : Date a -> Maybe Int
weekOfWeekBasedYear (Date { data, ops }) =
    let
        doy =
            ops.dayOfYear data

        lastWeek wOps =
            (6 + (startOfWeekOffset ops wOps data) + (ops.daysInYear data) + wOps.minimalDaysInFirstWeek) // 7

        woy wOps =
            week ops wOps data
    in
        case ops.weekOps of
            Just wOps ->
                if woy wOps >= lastWeek wOps then
                    Just ((woy wOps) - (lastWeek wOps) + 1)
                else if woy wOps > 0 then
                    Just (woy wOps)
                else
                    weekOfWeekBasedYear <|
                        asDate ops (ops.addDays (-doy - 1) data)

            Nothing ->
                Nothing


{-| weekBasedYear gives the year as determined when arranging years by whole
weeks.

While mostly the same as the calendar year, this will differ around new years.
Use this with weekOfWeekBasedYear. If a week 0 is acceptable or expected, use
weekOfYear and year.
-}
weekBasedYear : Date a -> Maybe Int
weekBasedYear (Date { data, ops }) =
    let
        yr =
            ops.year data

        doy =
            ops.dayOfYear data

        lastWeek wOps =
            (6 + (startOfWeekOffset ops wOps data) + (ops.daysInYear data) + wOps.minimalDaysInFirstWeek) // 7

        woy wOps =
            week ops wOps data

        calcYear wOps =
            if woy wOps == 0 then
                yr - 1
            else if woy wOps >= lastWeek wOps then
                yr + 1
            else
                yr
    in
        case ops.weekOps of
            Just wOps ->
                Just (calcYear wOps)

            Nothing ->
                Nothing
