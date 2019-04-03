module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import DateFormat
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (..)
import SetTheoryClock as STC exposing (Color(..), Light, LightType(..), Row, RowType(..))
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
view model =
    Element.layout [ Background.color (Element.rgb255 0 0 0), Element.padding 50 ]
        (Element.column
            [ Element.centerX, Element.spacing 14 ]
            (List.map
                (\row ->
                    Element.row
                        [ Element.centerX ]
                        (List.indexedMap
                            (\index light ->
                                let
                                    isOn =
                                        STC.isOn model row index
                                in
                                Element.el
                                    ([]
                                        |> padding row.rowType
                                        |> border light.lightType
                                        |> color light.color isOn
                                    )
                                    Element.none
                            )
                            row.lights
                        )
                )
                STC.rows
                ++ [ textClock model ]
            )
        )


padding : STC.RowType -> List (Element.Attribute msg) -> List (Element.Attribute msg)
padding rowType list =
    case rowType of
        STC.Second ->
            Element.paddingXY 34 32 :: list

        STC.FiveMinute ->
            Element.paddingXY 11 20 :: list

        _ ->
            Element.paddingXY 34 20 :: list


border : STC.LightType -> List (Element.Attribute msg) -> List (Element.Attribute msg)
border light li =
    let
        list =
            Border.color (Element.rgb255 160 160 160) :: li
    in
    case light of
        STC.Round ->
            Border.rounded 40
                :: Border.width 4
                :: list

        STC.Start ->
            Border.roundEach
                { topLeft = 15
                , bottomLeft = 15
                , topRight = 0
                , bottomRight = 0
                }
                :: Border.widthEach { bottom = 4, top = 4, left = 4, right = 2 }
                :: list

        STC.End ->
            Border.roundEach
                { topLeft = 0
                , bottomLeft = 0
                , topRight = 15
                , bottomRight = 15
                }
                :: Border.widthEach { bottom = 4, top = 4, left = 2, right = 4 }
                :: list

        STC.Middle ->
            Border.widthEach { bottom = 4, top = 4, left = 2, right = 2 }
                :: list


color : STC.Color -> Bool -> List (Element.Attribute msg) -> List (Element.Attribute msg)
color c on list =
    let
        x =
            if on then
                230

            else
                50
    in
    case c of
        STC.Yellow ->
            Background.color (Element.rgb255 x x 0) :: list

        STC.Red ->
            Background.color (Element.rgb255 x 0 0) :: list


textClock : ( Time.Posix, Time.Zone ) -> Element msg
textClock ( time, zone ) =
    Element.row
        [ Element.padding 10
        , Element.centerX
        , Font.color (Element.rgb255 150 150 150)
        ]
        [ Element.text (formatter zone time) ]


formatter =
    DateFormat.format
        [ DateFormat.hourMilitaryFixed
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        , DateFormat.text ":"
        , DateFormat.secondFixed
        ]
