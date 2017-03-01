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
        )

{-| This module defines a Date type to represent a unique day in any calendar system.

Note that there is no ZonedDate module. Attaching a zone to a date makes no
sense, as conversion between zones is impossible without a time. To carry the
information, you can always resort to a pair or a ZonedDateTime. The same
applies to offsets.

# Dates
@docs Date, Weekday, day, month, year, daysInMonth, daysInYear, addDays, addMonths, addYears, isLeapYear

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


wrap : (GregorianDate.Data -> a) -> Gregorian -> a
wrap f =
    (\(Gregorian d) ->
        f d
    )


wrapArgs :
    (a -> GregorianDate.Data -> GregorianDate.Data)
    -> a
    -> Gregorian
    -> Gregorian
wrapArgs func =
    (\d -> wrap (func d))
        >> (\f -> Gregorian << f)


gregorianOps : DateOps Gregorian
gregorianOps =
    { day = wrap GregorianDate.day
    , month = wrap GregorianDate.month
    , year = wrap GregorianDate.year
    , addDays = wrapArgs GregorianDate.addDays
    , addMonths = wrapArgs GregorianDate.addMonths
    , addYears = wrapArgs GregorianDate.addYears
    , isLeapYear = Just (wrap (GregorianDate.isLeapYear << GregorianDate.year))
    , daysInMonth = wrap GregorianDate.daysInMonth
    , daysInYear = wrap GregorianDate.daysInYear
    , weekOps = Just isoWeekOps
    , toGregorian = (\d -> Date { data = d, ops = gregorianOps })
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


split : Date a -> ( a, DateOps a )
split (Date { data, ops }) =
    ( data, ops )


period : Date a -> Date a -> Period
period date1 date2 =
    let
        ( data1, ops1 ) =
            split date1

        ( data2, ops2 ) =
            split date2
    in
        ops1.compOps.period data1 data2


daysBetween : Date a -> Date a -> Int
daysBetween date1 date2 =
    let
        ( data1, ops1 ) =
            split date1

        ( data2, ops2 ) =
            split date2
    in
        ops1.compOps.daysBetween data1 data2


add : Period -> Date a -> Date a
add period (Date { data, ops }) =
    ops.compOps.add period data
        |> asDate ops


isBefore : Date a -> Date a -> Bool
isBefore date1 date2 =
    let
        ( data1, ops1 ) =
            split date1

        ( data2, ops2 ) =
            split date2
    in
        ops1.compOps.isBefore data1 data2


isAfter : Date a -> Date a -> Bool
isAfter date1 date2 =
    let
        ( data1, ops1 ) =
            split date1

        ( data2, ops2 ) =
            split date2
    in
        ops1.compOps.isAfter data1 data2
