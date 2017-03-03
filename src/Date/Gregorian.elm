module Date.Gregorian
    exposing
        ( Data
        , year
        , month
        , day
        , dayOfYear
        , weekday
        , addYears
        , addMonths
        , addDays
        , isValidDate
        , isLeapYear
        , daysInMonth
        , daysInYear
        , period
        , addPeriod
        , daysBetween
        , isBefore
        , isAfter
        )


type alias Data =
    { day : Int
    , month : Int
    , year : Int
    }


{-| year returns a Date's year as an Int.
-}
year : Data -> Int
year =
    .year


{-| month returns a Date's month as an Int. Guaranteed to be in the
range [1, 12].
-}
month : Data -> Int
month =
    .month


{-| day returns a Date's year as an Int. Guaranteed to be valid for
the Date's (year, month) pair and in the range [1, 31].
-}
day : Data -> Int
day =
    .day


dayOfYear : Data -> Int
dayOfYear { year, month, day } =
    (daysFromYearMonth year month) + day


{-| weekday returns the day of week for a given Date.

This uses Sakamoto's method to determine the day of week. Sunday is 0, monday is
1, etc.
-}
weekday : Data -> Int
weekday { day, month, year } =
    let
        m =
            if month == 1 then
                0
            else if month == 2 then
                3
            else if month == 3 then
                2
            else if month == 4 then
                5
            else if month == 5 then
                0
            else if month == 6 then
                3
            else if month == 7 then
                5
            else if month == 8 then
                1
            else if month == 9 then
                4
            else if month == 10 then
                6
            else if month == 11 then
                2
            else
                4

        y =
            if month < 3 then
                year - 1
            else
                year
    in
        (y + y // 4 - y // 100 + y // 400 + m + day) % 7


{-| addYears adds a relative number (positive or negative) of years to
a Date, ensuring that the return value represents a valid Date.  If
the new date is not valid, days are subtracted from it until a valid
Date can be produced.
-}
addYears : Int -> Data -> Data
addYears years data =
    firstValid { data | year = data.year + years }


{-| addMonths adds a relative number (positive or negative) of months to
a Date, ensuring that the return value represents a valid Date.  Its
semantics are the same as `addYears`.
-}
addMonths : Int -> Data -> Data
addMonths months { year, month, day } =
    if months == 0 then
        { year = year
        , month = month
        , day = day
        }
    else
        let
            ms =
                year * 12 + month - 1 + months

            yo =
                if ms < 0 then
                    -1
                else
                    0
        in
            firstValid
                { year = ((ms - yo) // 12) + yo
                , month = (ms % 12) + 1
                , day = day
                }


{-| days adds an exact number (positive or negative) of days to a
Date.  Adding or subtracting days always produces a valid Date so
there is no fuzzing logic here like there is in `add{Months,Years}`.
-}
addDays : Int -> Data -> Data
addDays days data =
    daysFromYearMonthDay data
        |> ((+) days)
        |> dateFromDays


{-| isValidDate returns True if the given year, month and day
represent a valid date.
-}
isValidDate : Data -> Bool
isValidDate { year, month, day } =
    let
        days =
            unsafeDaysInMonth year month
    in
        if days < 0 then
            False
        else
            day >= 1 && day <= days


{-| isLeapYear returns True if the given year is a leap year.  The
rules for leap years are as follows:

* A year that is a multiple of 400 is a leap year.
* A year that is a multiple of 100 but not of 400 is not a leap year.
* A year that is a multiple of 4 but not of 100 is a leap year.
-}
isLeapYear : Int -> Bool
isLeapYear y =
    y % 400 == 0 || y % 100 /= 0 && y % 4 == 0


{-| daysInYear returns the number of days in the given year.
-}
daysInYear : Data -> Int
daysInYear { year, month, day } =
    if isLeapYear year then
        366
    else
        365


{-| daysInMonth returns the number of days in a month given a specific
year, taking leap years into account.

* A regular year has 365 days and the corresponding February has 28 days.
* A leap year has 366 days and the corresponding February has 29 days.
-}
daysInMonth : Data -> Int
daysInMonth { year, month, day } =
    unsafeDaysInMonth year month


unsafeDaysInMonth : Int -> Int -> Int
unsafeDaysInMonth y m =
    if m == 1 then
        31
    else if m == 2 && isLeapYear y then
        29
    else if m == 2 then
        28
    else if m == 3 then
        31
    else if m == 4 then
        30
    else if m == 5 then
        31
    else if m == 6 then
        30
    else if m == 7 then
        31
    else if m == 8 then
        31
    else if m == 9 then
        30
    else if m == 10 then
        31
    else if m == 11 then
        30
    else if m == 12 then
        31
    else
        -1


firstValid : Data -> Data
firstValid data =
    let
        minusDays ds dt =
            { dt | day = dt.day - ds }

        minusOne =
            minusDays 1 data

        minusTwo =
            minusDays 2 data

        minusThree =
            minusDays 3 data
    in
        if isValidDate data then
            data
        else if isValidDate minusOne then
            minusOne
        else if isValidDate minusTwo then
            minusTwo
        else
            minusThree


daysFromYearMonthDay : Data -> Int
daysFromYearMonthDay { day, month, year } =
    let
        yds =
            daysFromYear year

        mds =
            daysFromYearMonth year month

        dds =
            day - 1
    in
        yds + mds + dds


daysFromYearMonth : Int -> Int -> Int
daysFromYearMonth year month =
    let
        go year month acc =
            if month == 0 then
                acc
            else
                go year (month - 1) (acc + unsafeDaysInMonth year month)
    in
        go year (month - 1) 0


daysFromYear : Int -> Int
daysFromYear y =
    if y > 0 then
        366
            + ((y - 1) * 365)
            + ((y - 1) // 4)
            - ((y - 1) // 100)
            + ((y - 1) // 400)
    else if y < 0 then
        (y * 365)
            + (y // 4)
            - (y // 100)
            + (y // 400)
    else
        0


yearFromDays : Int -> Int
yearFromDays ds =
    let
        y =
            ds // 365

        d =
            daysFromYear y
    in
        if ds <= d then
            y - 1
        else
            y


dateFromDays : Int -> Data
dateFromDays ds =
    let
        d400 =
            daysFromYear 400

        y400 =
            ds // d400

        d =
            rem ds d400

        year =
            yearFromDays (d + 1)

        leap =
            if isLeapYear year then
                ((+) 1)
            else
                identity

        doy =
            d - daysFromYear year

        ( month, day ) =
            if doy < 31 then
                ( 1, doy + 1 )
            else if doy < leap 59 then
                ( 2, doy - 31 + 1 )
            else if doy < leap 90 then
                ( 3, doy - leap 59 + 1 )
            else if doy < leap 120 then
                ( 4, doy - leap 90 + 1 )
            else if doy < leap 151 then
                ( 5, doy - leap 120 + 1 )
            else if doy < leap 181 then
                ( 6, doy - leap 151 + 1 )
            else if doy < leap 212 then
                ( 7, doy - leap 181 + 1 )
            else if doy < leap 243 then
                ( 8, doy - leap 212 + 1 )
            else if doy < leap 273 then
                ( 9, doy - leap 243 + 1 )
            else if doy < leap 304 then
                ( 10, doy - leap 273 + 1 )
            else if doy < leap 334 then
                ( 11, doy - leap 304 + 1 )
            else
                ( 12, doy - leap 334 + 1 )
    in
        { year = year + y400 * 400
        , month = month
        , day = day
        }


{-| period returns the relative number of years, months and days between two
Dates, in this order.
-}
period : Data -> Data -> ( Int, Int, Int )
period data1 data2 =
    let
        dy =
            data2.year - data1.year

        years =
            if isAfter (addYears dy data1) data2 then
                dy - 1
            else
                dy

        cy =
            addYears years data1

        dm =
            (data2.month - cy.month) % 12

        months =
            if isAfter (addMonths dm cy) data2 then
                dm - 1
            else
                dm

        days =
            daysBetween (addMonths months cy) data2
    in
        ( years
        , months
        , days
        )


{-| addPeriod adds the number of years, months and days to the date.
-}
addPeriod : ( Int, Int, Int ) -> Data -> Data
addPeriod ( years, months, days ) =
    addYears years
        >> addMonths months
        >> addDays days


daysBetween : Data -> Data -> Int
daysBetween data1 data2 =
    daysFromYearMonthDay data2 - daysFromYearMonthDay data1


isBefore : Data -> Data -> Bool
isBefore data1 data2 =
    if data1.year < data2.year then
        True
    else if data1.year == data2.year && data1.month < data2.month then
        True
    else if data1.year == data2.year && data1.month == data2.month && data1.day < data2.day then
        True
    else
        False


isAfter : Data -> Data -> Bool
isAfter data1 data2 =
    if data1.year > data2.year then
        True
    else if data1.year == data2.year && data1.month > data2.month then
        True
    else if data1.year == data2.year && data1.month == data2.month && data1.day > data2.day then
        True
    else
        False
