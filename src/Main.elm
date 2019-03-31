module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import DateFormat
import Element exposing (Element, alignRight, centerY, column, el, fill, height, maximum, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (..)
import SetTheoryClock
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    ( Time.Posix, Time.Zone )


init : () -> ( Model, Cmd Msg )
init _ =
    ( ( Time.millisToPosix 0, Time.utc )
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( time, zone ) =
    case msg of
        Tick newTime ->
            ( ( newTime, zone )
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( ( time, newZone )
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view ( time, zone ) =
    let
        blink =
            SetTheoryClock.oddSecond ( time, zone )

        fiveHour =
            SetTheoryClock.fiveHour ( time, zone )

        oneHour =
            SetTheoryClock.oneHour ( time, zone )

        oneMinute =
            SetTheoryClock.oneMinute ( time, zone )

        fiveMinute =
            SetTheoryClock.fiveMinute ( time, zone )
    in
    Element.layout [ Background.color (rgb255 0 0 0), Element.padding 50 ]
        (column [ Element.centerX, spacing 14 ]
            [ row [ Element.centerX ]
                [ el (yellow blink |> roundPadding |> addBorder Singel) Element.none
                ]
            , row []
                [ el (red (fiveHour 0) |> normalPadding |> addBorder Start) Element.none
                , el (red (fiveHour 1) |> normalPadding |> addBorder Middle) Element.none
                , el (red (fiveHour 2) |> normalPadding |> addBorder Middle) Element.none
                , el (red (fiveHour 3) |> normalPadding |> addBorder End) Element.none
                ]
            , row []
                [ el (red (oneHour 0) |> normalPadding |> addBorder Start) Element.none
                , el (red (oneHour 1) |> normalPadding |> addBorder Middle) Element.none
                , el (red (oneHour 2) |> normalPadding |> addBorder Middle) Element.none
                , el (red (oneHour 3) |> normalPadding |> addBorder End) Element.none
                ]
            , row []
                [ el (yellow (fiveMinute 0) |> smallPadding |> addBorder Start) Element.none
                , el (yellow (fiveMinute 1) |> smallPadding |> addBorder Middle) Element.none
                , el (red (fiveMinute 2) |> smallPadding |> addBorder Middle) Element.none
                , el (yellow (fiveMinute 3) |> smallPadding |> addBorder Middle) Element.none
                , el (yellow (fiveMinute 4) |> smallPadding |> addBorder Middle) Element.none
                , el (red (fiveMinute 5) |> smallPadding |> addBorder Middle) Element.none
                , el (yellow (fiveMinute 6) |> smallPadding |> addBorder Middle) Element.none
                , el (yellow (fiveMinute 7) |> smallPadding |> addBorder Middle) Element.none
                , el (red (fiveMinute 8) |> smallPadding |> addBorder Middle) Element.none
                , el (yellow (fiveMinute 9) |> smallPadding |> addBorder Middle) Element.none
                , el (yellow (fiveMinute 10) |> smallPadding |> addBorder Middle) Element.none
                , el (red (fiveMinute 11) |> smallPadding |> addBorder End) Element.none
                ]
            , row []
                [ el (yellow (oneMinute 0) |> normalPadding |> addBorder Start) Element.none
                , el (yellow (oneMinute 1) |> normalPadding |> addBorder Middle) Element.none
                , el (yellow (oneMinute 2) |> normalPadding |> addBorder Middle) Element.none
                , el (yellow (oneMinute 3) |> normalPadding |> addBorder End) Element.none
                ]
            , row [ Element.centerX ]
                [ el [ Font.color (rgb255 200 200 200), Element.padding 20 ]
                    (Element.text (formatter zone time))
                ]
            ]
        )


normalPadding list =
    Element.paddingXY 34 22 :: list


roundPadding list =
    Border.rounded 40 :: (Element.padding 34 :: list)


smallPadding list =
    Element.paddingXY 10 22 :: list


type StcBorder
    = Start
    | Middle
    | End
    | Singel


addBorder borderType list =
    let
        b =
            Border.color (rgb255 200 200 200) :: list
    in
    case borderType of
        Start ->
            Border.roundEach
                { topLeft = 15
                , bottomLeft = 15
                , topRight = 0
                , bottomRight = 0
                }
                :: (Border.widthEach { bottom = 4, top = 4, left = 4, right = 2 } :: b)

        Middle ->
            Border.widthEach
                { bottom = 4
                , top = 4
                , left = 2
                , right = 2
                }
                :: b

        End ->
            Border.roundEach
                { topLeft = 0
                , bottomLeft = 0
                , topRight = 15
                , bottomRight = 15
                }
                :: (Border.widthEach { bottom = 4, top = 4, left = 2, right = 4 } :: b)

        Singel ->
            Border.widthEach
                { bottom = 4
                , top = 4
                , left = 4
                , right = 4
                }
                :: b


yellow bool =
    case bool of
        True ->
            [ Background.color (rgb255 230 230 50) ]

        False ->
            [ Background.color (rgb255 90 90 10) ]


red b =
    case b of
        True ->
            [ Background.color (rgb255 255 50 50) ]

        False ->
            [ Background.color (rgb255 50 30 30) ]


formatter =
    DateFormat.format
        [ DateFormat.hourMilitaryFixed
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        , DateFormat.text ":"
        , DateFormat.secondFixed
        ]
