module SetTheoryClock exposing
    ( Color(..)
    , LightType(..)
    , RowType
    , fiveHour
    , fiveMinute
    , isOn
    , lights
    , oddSecond
    , oneHour
    , oneMinute
    , rows
    )

import Time


type RowType
    = Second
    | FiveHour
    | OneHour
    | FiveMinute
    | OneMinute


type LightType
    = Round Color
    | Start Color
    | Middle Color
    | End Color


type Color
    = Yellow
    | Red


lights : RowType -> List LightType
lights row =
    case row of
        Second ->
            [ Round Yellow ]

        FiveHour ->
            [ Start Red, Middle Red, Middle Red, End Red ]

        OneHour ->
            [ Start Red, Middle Red, Middle Red, End Red ]

        FiveMinute ->
            [ Start Yellow
            , Middle Yellow
            , Middle Red
            , Middle Yellow
            , Middle Yellow
            , Middle Red
            , Middle Yellow
            , Middle Yellow
            , Middle Red
            , Middle Yellow
            , End Yellow
            ]

        OneMinute ->
            [ Start Yellow, Middle Yellow, Middle Yellow, End Yellow ]


rows : List RowType
rows =
    [ Second, FiveHour, OneHour, FiveMinute, OneMinute ]


isOn : ( Time.Posix, Time.Zone ) -> RowType -> Int -> Bool
isOn tz rowType index =
    case rowType of
        Second ->
            oddSecond tz

        FiveHour ->
            fiveHour tz index

        OneHour ->
            oneHour tz index

        FiveMinute ->
            fiveMinute tz index

        OneMinute ->
            oneMinute tz index


fiveHour : ( Time.Posix, Time.Zone ) -> Int -> Bool
fiveHour ( time, zone ) index =
    index < Time.toHour zone time // 5


oneHour : ( Time.Posix, Time.Zone ) -> Int -> Bool
oneHour ( time, zone ) index =
    index < modBy 5 (Time.toHour zone time)


oneMinute : ( Time.Posix, Time.Zone ) -> Int -> Bool
oneMinute ( time, zone ) index =
    index < modBy 5 (Time.toMinute zone time)


fiveMinute : ( Time.Posix, Time.Zone ) -> Int -> Bool
fiveMinute ( time, zone ) index =
    index < Time.toMinute zone time // 5


oddSecond : ( Time.Posix, Time.Zone ) -> Bool
oddSecond ( time, zone ) =
    case modBy 2 (Time.toSecond zone time) of
        0 ->
            False

        _ ->
            True
