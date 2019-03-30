module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
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

        startBorder =
            Border.roundEach { topLeft = 15, bottomLeft = 15, topRight = 0, bottomRight = 0 }
    in
    Element.layout [ Background.color (rgb255 0 0 0) ]
        (column [ Element.centerX, height (fill |> Element.minimum 1000), spacing 4 ]
            [ row [ padding 10, Element.centerX ]
                [ el [ yelloWhenSet blink, Border.rounded 32, Element.padding 30, borderWidht, borderColor ] Element.none
                ]
            , row []
                [ el [ redWhenSet (fiveHour 0), standardPadding, borderWidht, borderColor, startBorder ] Element.none
                , el [ redWhenSet (fiveHour 1), standardPadding, borderWidht, borderColor ] Element.none
                , el [ redWhenSet (fiveHour 2), standardPadding, borderWidht, borderColor ] Element.none
                , el [ redWhenSet (fiveHour 3), standardPadding, borderWidht, borderColor, endBorder ] Element.none
                ]
            , row []
                [ el [ redWhenSet (oneHour 0), standardPadding, borderWidht, borderColor, startBorder ] Element.none
                , el [ redWhenSet (oneHour 1), standardPadding, borderWidht, borderColor ] Element.none
                , el [ redWhenSet (oneHour 2), standardPadding, borderWidht, borderColor ] Element.none
                , el [ redWhenSet (oneHour 3), standardPadding, borderWidht, borderColor, endBorder ] Element.none
                ]
            , row []
                [ el [ yelloWhenSet (fiveMinute 0), smallPadding, borderWidht, borderColor, startBorder ] Element.none
                , el [ yelloWhenSet (fiveMinute 1), smallPadding, borderWidht, borderColor ] Element.none
                , el [ redWhenSet (fiveMinute 2), smallPadding, borderWidht, borderColor ] Element.none
                , el [ yelloWhenSet (fiveMinute 3), smallPadding, borderWidht, borderColor ] Element.none
                , el [ yelloWhenSet (fiveMinute 4), smallPadding, borderWidht, borderColor ] Element.none
                , el [ redWhenSet (fiveMinute 5), smallPadding, borderWidht, borderColor ] Element.none
                , el [ yelloWhenSet (fiveMinute 6), smallPadding, borderWidht, borderColor ] Element.none
                , el [ yelloWhenSet (fiveMinute 7), smallPadding, borderWidht, borderColor ] Element.none
                , el [ redWhenSet (fiveMinute 8), smallPadding, borderWidht, borderColor ] Element.none
                , el [ yelloWhenSet (fiveMinute 9), smallPadding, borderWidht, borderColor ] Element.none
                , el [ yelloWhenSet (fiveMinute 10), smallPadding, borderWidht, borderColor, endBorder ] Element.none
                ]
            , row []
                [ el [ yelloWhenSet (oneMinute 0), standardPadding, borderWidht, borderColor, startBorder ] Element.none
                , el [ yelloWhenSet (oneMinute 1), standardPadding, borderWidht, borderColor ] Element.none
                , el [ yelloWhenSet (oneMinute 2), standardPadding, borderWidht, borderColor ] Element.none
                , el [ yelloWhenSet (oneMinute 3), standardPadding, borderWidht, borderColor, endBorder ] Element.none
                ]
            ]
        )


endBorder =
    Border.roundEach { topLeft = 0, bottomLeft = 0, topRight = 15, bottomRight = 15 }


standardPadding =
    Element.paddingXY 30 20


smallPadding =
    Element.paddingXY 9 20


borderColor =
    Border.color (rgb255 200 200 200)


borderWidht =
    Border.width 3


yelloWhenSet : Bool -> Element.Attribute msg
yelloWhenSet b =
    case b of
        True ->
            Background.color (rgb255 230 230 50)

        False ->
            Background.color (rgb255 90 90 10)


redWhenSet : Bool -> Element.Attribute msg
redWhenSet b =
    case b of
        True ->
            Background.color (rgb255 255 50 50)

        False ->
            Background.color (rgb255 50 30 30)



-- h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second ++ "  " ++ blink ++ "  " ++ fiveMinutes1) ]
