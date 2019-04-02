module SetTheoryClock exposing
    ( Color(..)
    , LightType(..)
    , RowType(..)
    , isOn
    , lights
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
isOn ( time, zone ) rowType index =
    case rowType of
        Second ->
            case modBy 2 (Time.toSecond zone time) of
                0 ->
                    False

                _ ->
                    True

        FiveHour ->
            index < Time.toHour zone time // 5

        OneHour ->
            index < modBy 5 (Time.toHour zone time)

        FiveMinute ->
            index < Time.toMinute zone time // 5

        OneMinute ->
            index < modBy 5 (Time.toMinute zone time)
