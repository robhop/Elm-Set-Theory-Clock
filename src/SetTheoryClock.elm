module SetTheoryClock exposing (fiveHour, fiveMinute, oddSecond, oneHour, oneMinute)

import Time


fiveHour : ( Time.Posix, Time.Zone ) -> Int -> Bool
fiveHour ( time, zone ) step =
    step < Time.toHour zone time // 5


oneHour : ( Time.Posix, Time.Zone ) -> Int -> Bool
oneHour ( time, zone ) step =
    step < modBy 5 (Time.toHour zone time)


oneMinute : ( Time.Posix, Time.Zone ) -> Int -> Bool
oneMinute ( time, zone ) step =
    step < modBy 5 (Time.toMinute zone time)


fiveMinute : ( Time.Posix, Time.Zone ) -> Int -> Bool
fiveMinute ( time, zone ) step =
    step < Time.toMinute zone time // 5


oddSecond : ( Time.Posix, Time.Zone ) -> Bool
oddSecond ( time, zone ) =
    case modBy 2 (Time.toSecond zone time) of
        0 ->
            False

        _ ->
            True
