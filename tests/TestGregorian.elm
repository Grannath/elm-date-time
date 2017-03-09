module TestGregorian exposing (..)

import TestUtils exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (int, intRange, Fuzzer)
import Test exposing (..)
import Date.Date exposing (..)


all : Test
all =
    describe "Gregorian dates"
        [ constructing
        , leapYears
        , adders
        , lengths
        , weeks
        , comparisons
        ]


constructing : Test
constructing =
    describe "Date.Date.gregorian"
        [ fuzz yearMonthDayFuzzer "constructs dates from valid input" <|
            \( year, month, day ) ->
                datesEqual (date year month day) ( year, month, day )
        , describe "fails on invalid input"
            [ fuzz yearMonthDayFuzzer "does not allow months larger than 12" <|
                \( year, month, day ) ->
                    fails
                        ("Returned Date from invalid input "
                            ++ (print year month day)
                            ++ "."
                        )
                        (gregorian year (month + 12) day)
            , fuzz yearMonthDayFuzzer "does not allow days larger than the months maximum" <|
                \( year, month, day ) ->
                    fails
                        ("Returned Date from invalid input "
                            ++ (print year month day)
                            ++ "."
                        )
                        (gregorian year month (day + 31))
            ]
        ]


leapYears : Test
leapYears =
    describe "leap years"
        [ describe "Date.Date.gregorian"
            [ fuzz (intRange -400 2020) "distinguishes between leap years" <|
                \year ->
                    if List.member year validLeapYears then
                        works
                            "Failed for 29. February on a leap year."
                            (gregorian year 2 29)
                    else
                        fails
                            "Worked for 29. February on a non-leap year."
                            (gregorian year 2 29)
            ]
        , describe "Date.Date.isLeapYear"
            [ fuzz (intRange -400 2020) "is correct given any year" <|
                \year ->
                    if List.member year validLeapYears then
                        Expect.true
                            ("Expected " ++ toString year ++ " to be a leap year")
                            (isLeapYear (date year 1 1))
                    else
                        Expect.false
                            (toString year ++ " is not a leap year")
                            (isLeapYear (date year 1 1))
            ]
        ]


adders : Test
adders =
    describe "Date.Date.add{Years,Months,Days}"
        [ fuzz2 (intRange -1000 1000) dateFuzzer "addYears is relative" <|
            \years date1 ->
                let
                    date2 =
                        addYears years date1
                in
                    Expect.equal years ((year date2) - (year date1))
        , fuzz2 (intRange -1000 1000) dateFuzzer "addMonths is relative" <|
            \months date1 ->
                let
                    date2 =
                        addMonths months date1

                    dY =
                        ((year date2) - (year date1))

                    dM =
                        ((month date2) - (month date1))
                in
                    Expect.equal months (dY * 12 + dM)
        , fuzz2 (intRange -1000 1000) dateFuzzer "addDays is reversible" <|
            \days date1 ->
                let
                    date2 =
                        addDays days date1
                in
                    Expect.equal date1 (addDays -days date2)
        ]


lengths : Test
lengths =
    describe "Date.Date.daysIn{Month,Year}"
        [ describe "Date.Date.daysInMonth"
            [ fuzz2 (intRange -400 2020) (intRange 1 12) "is correct given any year, month pair" <|
                \year month ->
                    daysInMonth (date year month 1)
                        |> Expect.equal (monthDays year month)
            ]
        , describe "Date.Date.daysInYear"
            [ fuzz (intRange -400 2020) "is correct given any year" <|
                \year ->
                    let
                        days =
                            if List.member year validLeapYears then
                                366
                            else
                                365
                    in
                        daysInYear (date year 1 1)
                            |> Expect.equal days
            ]
        ]


yearDay : Test
yearDay =
    describe "Date.Date.dayOfYear"
        [ fuzz2 (intRange -400 2020) (intRange 1 365) "works for every year" <|
            \year day ->
                let
                    date1 =
                        date year 1 1

                    date2 =
                        addDays (day - 1) date1
                in
                    Expect.equal day (dayOfYear date2)
        ]


weekData : List ( Date Gregorian, Weekday, Int, Int, Int )
weekData =
    [ ( date 1969 12 29, Mon, 53, 1, 1970 )
    , ( date 2008 12 31, Wed, 53, 1, 2009 )
    , ( date 2009 1 1, Thu, 1, 1, 2009 )
    , ( date 2009 1 2, Fri, 1, 1, 2009 )
    , ( date 2009 1 5, Mon, 2, 2, 2009 )
    , ( date 2009 12 31, Thu, 53, 53, 2009 )
    , ( date 2010 1 1, Fri, 0, 53, 2009 )
    , ( date 2010 1 2, Sat, 0, 53, 2009 )
    , ( date 2010 1 4, Mon, 1, 1, 2010 )
    , ( date 2010 12 31, Fri, 52, 52, 2010 )
    , ( date 2011 1 1, Sat, 0, 52, 2010 )
    , ( date 2011 1 2, Sun, 0, 52, 2010 )
    , ( date 2011 1 3, Mon, 1, 1, 2011 )
    , ( date 2012 12 23, Sun, 51, 51, 2012 )
    , ( date 2012 12 24, Mon, 52, 52, 2012 )
    , ( date 2012 12 27, Thu, 52, 52, 2012 )
    , ( date 2012 12 28, Fri, 52, 52, 2012 )
    , ( date 2012 12 29, Sat, 52, 52, 2012 )
    , ( date 2012 12 30, Sun, 52, 52, 2012 )
    , ( date 2012 12 31, Mon, 53, 1, 2013 )
    , ( date 2013 1 1, Tue, 1, 1, 2013 )
    , ( date 2013 1 2, Wed, 1, 1, 2013 )
    , ( date 2013 1 6, Sun, 1, 1, 2013 )
    , ( date 2013 1 7, Mon, 2, 2, 2013 )
    ]


weeks : Test
weeks =
    describe "calendar weeks"
        [ testAll weekData "Date.Date.dayOfWeek returns the correct Weekday" <|
            \( date, day, _, _, _ ) ->
                Expect.true
                    ("Date "
                        ++ (printDate date)
                        ++ " should have day "
                        ++ (toString day)
                        ++ ", but has "
                        ++ (toString <| dayOfWeek date)
                    )
                    (Just day == dayOfWeek date)
        , testAll weekData "Date.Date.weekOfWeekBasedYear returns the correct week" <|
            \( date, _, _, week, _ ) ->
                Expect.true
                    ("Date "
                        ++ (printDate date)
                        ++ " should have weekOfWeekBasedYear "
                        ++ (toString week)
                        ++ ", but has "
                        ++ (toString <| weekOfWeekBasedYear date)
                    )
                    (Just week == weekOfWeekBasedYear date)
        , testAll weekData "Date.Date.weekBasedYear returns the correct year" <|
            \( date, _, _, _, year ) ->
                Expect.true
                    ("Date "
                        ++ (printDate date)
                        ++ " should have weekBasedYear "
                        ++ (toString year)
                        ++ ", but has "
                        ++ (toString <| weekBasedYear date)
                    )
                    (Just year == weekBasedYear date)
        , testAll weekData "Date.Date.weekOfYear returns the correct week" <|
            \( date, _, week, _, _ ) ->
                Expect.true
                    ("Date "
                        ++ (printDate date)
                        ++ " should have weekOfYear "
                        ++ (toString week)
                        ++ ", but has "
                        ++ (toString <| weekOfYear date)
                    )
                    (Just week == weekOfYear date)
        ]


comparisons : Test
comparisons =
    describe "date comparisons"
        [ describe "Date.Date.isBefore"
            [ fuzz2 (intRange -1000 1000) dateFuzzer "works with any two dates" <|
                \days date ->
                    let
                        date2 =
                            addDays days date
                    in
                        if days > 0 then
                            Expect.true
                                ("Date "
                                    ++ (printDate date)
                                    ++ " should be before a date "
                                    ++ (toString days)
                                    ++ " days later, but it is not."
                                )
                                (isBefore date date2)
                        else
                            Expect.false
                                ("Date "
                                    ++ (printDate date)
                                    ++ " should not be before a date "
                                    ++ (toString days)
                                    ++ " days later, but it is."
                                )
                                (isBefore date date2)
            ]
        , describe "Date.Date.isAfter"
            [ fuzz2 (intRange -1000 1000) dateFuzzer "works with any two dates" <|
                \days date ->
                    let
                        date2 =
                            addDays days date
                    in
                        if days < 0 then
                            Expect.true
                                ("Date "
                                    ++ (printDate date)
                                    ++ " should be after a date "
                                    ++ (toString -days)
                                    ++ " days before, but it is not."
                                )
                                (isAfter date date2)
                        else
                            Expect.false
                                ("Date "
                                    ++ (printDate date)
                                    ++ " should not be after a date "
                                    ++ (toString -days)
                                    ++ " days before, but it is."
                                )
                                (isAfter date date2)
            ]
        , describe "Date.Date.daysBetween"
            [ fuzz2 (intRange -1000 1000) dateFuzzer "works with any two dates" <|
                \days date ->
                    let
                        date2 =
                            addDays days date
                    in
                        Expect.true
                            ("Date "
                                ++ (printDate date)
                                ++ " and date "
                                ++ (printDate date2)
                                ++ " should be "
                                ++ (toString days)
                                ++ " days apart, but daysBetween gives "
                                ++ (toString <| daysBetween date date2)
                                ++ " days."
                            )
                            (daysBetween date date2 == days)
            ]
        , describe "Date.Date.period and add are inverse"
            [ fuzz2 (intRange -1000 1000) dateFuzzer "works with any two dates" <|
                \days date ->
                    let
                        date2 =
                            addDays days date

                        prd =
                            period date date2
                    in
                        Expect.true
                            ("Date "
                                ++ (printDate date)
                                ++ " and date "
                                ++ (printDate date2)
                                ++ " result in a period of "
                                ++ (toString prd)
                                ++ ", which added to the first date results in "
                                ++ (printDate <| add prd date)
                                ++ "."
                            )
                            (add prd date == date2)
            ]
        ]



-- Test utils and data


printDate : Date a -> String
printDate date =
    print (year date) (month date) (day date)


print : Int -> Int -> Int -> String
print year month day =
    (toString year)
        ++ "-"
        ++ (toString month)
        ++ "-"
        ++ (toString day)


toTuple : Date a -> ( Int, Int, Int )
toTuple date =
    ( year date, month date, day date )


datesEqual : Date a -> ( Int, Int, Int ) -> Expect.Expectation
datesEqual date dateTuple =
    Expect.equal (toTuple date) dateTuple


validLeapYears : List Int
validLeapYears =
    List.concat
        [ [ -400, -396, -392, -388, -384, -380, -376, -372, -368, -364, -360, -356, -352, -348, -344, -340, -336, -332, -328, -324, -320, -316, -312, -308, -304, -296, -292, -288, -284, -280 ]
        , [ -276, -272, -268, -264, -260, -256, -252, -248, -244, -240, -236, -232, -228, -224, -220, -216, -212, -208, -204, -196, -192, -188, -184, -180, -176, -172, -168, -164, -160, -156 ]
        , [ -152, -148, -144, -140, -136, -132, -128, -124, -120, -116, -112, -108, -104, -96, -92, -88, -84, -80, -76, -72, -68, -64, -60, -56, -52, -48, -44, -40, -36, -32 ]
        , [ -28, -24, -20, -16, -12, -8, -4, 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72, 76, 80, 84, 88 ]
        , [ 92, 96, 104, 108, 112, 116, 120, 124, 128, 132, 136, 140, 144, 148, 152, 156, 160, 164, 168, 172, 176, 180, 184, 188, 192, 196, 204, 208, 212, 216 ]
        , [ 220, 224, 228, 232, 236, 240, 244, 248, 252, 256, 260, 264, 268, 272, 276, 280, 284, 288, 292, 296, 304, 308, 312, 316, 320, 324, 328, 332, 336, 340 ]
        , [ 344, 348, 352, 356, 360, 364, 368, 372, 376, 380, 384, 388, 392, 396, 400, 404, 408, 412, 416, 420, 424, 428, 432, 436, 440, 444, 448, 452, 456, 460 ]
        , [ 464, 468, 472, 476, 480, 484, 488, 492, 496, 504, 508, 512, 516, 520, 524, 528, 532, 536, 540, 544, 548, 552, 556, 560, 564, 568, 572, 576, 580, 584 ]
        , [ 588, 592, 596, 604, 608, 612, 616, 620, 624, 628, 632, 636, 640, 644, 648, 652, 656, 660, 664, 668, 672, 676, 680, 684, 688, 692, 696, 704, 708, 712 ]
        , [ 716, 720, 724, 728, 732, 736, 740, 744, 748, 752, 756, 760, 764, 768, 772, 776, 780, 784, 788, 792, 796, 800, 804, 808, 812, 816, 820, 824, 828, 832 ]
        , [ 836, 840, 844, 848, 852, 856, 860, 864, 868, 872, 876, 880, 884, 888, 892, 896, 904, 908, 912, 916, 920, 924, 928, 932, 936, 940, 944, 948, 952, 956 ]
        , [ 960, 964, 968, 972, 976, 980, 984, 988, 992, 996, 1004, 1008, 1012, 1016, 1020, 1024, 1028, 1032, 1036, 1040, 1044, 1048, 1052, 1056, 1060, 1064, 1068, 1072, 1076, 1080 ]
        , [ 1084, 1088, 1092, 1096, 1104, 1108, 1112, 1116, 1120, 1124, 1128, 1132, 1136, 1140, 1144, 1148, 1152, 1156, 1160, 1164, 1168, 1172, 1176, 1180, 1184, 1188, 1192, 1196, 1200, 1204 ]
        , [ 1208, 1212, 1216, 1220, 1224, 1228, 1232, 1236, 1240, 1244, 1248, 1252, 1256, 1260, 1264, 1268, 1272, 1276, 1280, 1284, 1288, 1292, 1296, 1304, 1308, 1312, 1316, 1320, 1324, 1328 ]
        , [ 1332, 1336, 1340, 1344, 1348, 1352, 1356, 1360, 1364, 1368, 1372, 1376, 1380, 1384, 1388, 1392, 1396, 1404, 1408, 1412, 1416, 1420, 1424, 1428, 1432, 1436, 1440, 1444, 1448, 1452 ]
        , [ 1456, 1460, 1464, 1468, 1472, 1476, 1480, 1484, 1488, 1492, 1496, 1504, 1508, 1512, 1516, 1520, 1524, 1528, 1532, 1536, 1540, 1544, 1548, 1552, 1556, 1560, 1564, 1568, 1572, 1576 ]
        , [ 1580, 1584, 1588, 1592, 1596, 1600, 1604, 1608, 1612, 1616, 1620, 1624, 1628, 1632, 1636, 1640, 1644, 1648, 1652, 1656, 1660, 1664, 1668, 1672, 1676, 1680, 1684, 1688, 1692, 1696 ]
        , [ 1704, 1708, 1712, 1716, 1720, 1724, 1728, 1732, 1736, 1740, 1744, 1748, 1752, 1756, 1760, 1764, 1768, 1772, 1776, 1780, 1784, 1788, 1792, 1796, 1804, 1808, 1812, 1816, 1820, 1824 ]
        , [ 1828, 1832, 1836, 1840, 1844, 1848, 1852, 1856, 1860, 1864, 1868, 1872, 1876, 1880, 1884, 1888, 1892, 1896, 1904, 1908, 1912, 1916, 1920, 1924, 1928, 1932, 1936, 1940, 1944, 1948 ]
        , [ 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020 ]
        ]


standardYearMonths : List Int
standardYearMonths =
    [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]


leapYearMonths : List Int
leapYearMonths =
    [ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]


monthDays : Int -> Int -> Int
monthDays year month =
    let
        monthDays =
            if isLeapYear (date year 1 1) then
                leapYearMonths
            else
                standardYearMonths
    in
        monthDays
            |> List.drop (month - 1)
            |> List.head
            |> Maybe.withDefault 0


fuzzDate : String -> (Int -> Int -> Int -> Expectation) -> Test
fuzzDate =
    fuzz3 int (intRange 1 12) (intRange 1 31)


yearMonthDayFuzzer : Fuzzer ( Int, Int, Int )
yearMonthDayFuzzer =
    Fuzz.tuple3 ( (intRange -400 4000), (intRange 1 12), (intRange 1 31) )
        |> Fuzz.map
            (\( year, month, day ) ->
                ( year, month, clamp 1 (monthDays year month) day )
            )


dateFuzzer : Fuzzer (Date Gregorian)
dateFuzzer =
    let
        tupleToDate =
            \( year, month, day ) ->
                date year month day
    in
        Fuzz.map tupleToDate yearMonthDayFuzzer


date : Int -> Int -> Int -> Date Gregorian
date year month day =
    let
        date =
            gregorian year month day
    in
        case date of
            Ok dt ->
                dt

            Err msg ->
                Debug.crash msg


works : String -> Result a b -> Expectation
works mes res =
    case res of
        Ok _ ->
            Expect.pass

        Err _ ->
            Expect.fail mes


fails : String -> Result a b -> Expectation
fails mes res =
    case res of
        Ok _ ->
            Expect.fail mes

        Err _ ->
            Expect.pass
