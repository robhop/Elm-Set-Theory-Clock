module SetTheoryClock exposing
    ( Color(..)
    , Light
    , LightType(..)
    , Row
    , RowType(..)
    , isOn
    , rows
    )

import Time


type alias Row =
    { rowType : RowType, lights : List Light }


type alias Light =
    { lightType : LightType, color : Color }


type RowType
    = Second
    | FiveHour
    | OneHour
    | FiveMinute
    | OneMinute


type LightType
    = Round
    | Start
    | Middle
    | End


type Color
    = Yellow
    | Red


rows : List Row
rows =
    [ Row Second
        [ Light Round Yellow
        ]
    , Row FiveHour
        [ Light Start Red
        , Light Middle Red
        , Light Middle Red
        , Light End Red
        ]
    , Row OneHour
        [ Light Start Red
        , Light Middle Red
        , Light Middle Red
        , Light End Red
        ]
    , Row FiveMinute
        [ Light Start Yellow
        , Light Middle Yellow
        , Light Middle Red
        , Light Middle Yellow
        , Light Middle Yellow
        , Light Middle Red
        , Light Middle Yellow
        , Light Middle Yellow
        , Light Middle Red
        , Light Middle Yellow
        , Light End Yellow
        ]
    , Row OneMinute
        [ Light Start Yellow
        , Light Middle Yellow
        , Light Middle Yellow
        , Light End Yellow
        ]
    ]


isOn : ( Time.Posix, Time.Zone ) -> Row -> Int -> Bool
isOn ( time, zone ) row index =
    case row.rowType of
        Second ->
            modBy 2 (Time.toSecond zone time) == 0

        FiveHour ->
            index < Time.toHour zone time // 5

        OneHour ->
            index < modBy 5 (Time.toHour zone time)

        FiveMinute ->
            index < Time.toMinute zone time // 5

        OneMinute ->
            index < modBy 5 (Time.toMinute zone time)
