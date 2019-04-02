module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (..)
import SetTheoryClock as STC exposing (Color(..), LightType(..), RowType(..))
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
view tz =
    Element.layout [ Background.color (Element.rgb255 0 0 0), Element.padding 50 ]
        (Element.column
            [ Element.centerX, Element.spacing 14 ]
            (List.map
                (\row ->
                    Element.row
                        [ Element.centerX ]
                        (List.indexedMap
                            (\index lightType ->
                                let
                                    isOn =
                                        STC.isOn tz row index

                                    element c =
                                        Element.el (padding row [] |> border lightType |> color c isOn) Element.none
                                in
                                case lightType of
                                    STC.Round c ->
                                        element c

                                    STC.Start c ->
                                        element c

                                    STC.End c ->
                                        element c

                                    STC.Middle c ->
                                        element c
                            )
                            (STC.lights row)
                        )
                )
                STC.rows
            )
        )


padding : STC.RowType -> List (Element.Attribute msg) -> List (Element.Attribute msg)
padding row list =
    case row of
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
            Border.color (Element.rgb255 200 200 200) :: li
    in
    case light of
        STC.Round c ->
            Border.rounded 40
                :: Border.width 4
                :: list

        STC.Start c ->
            Border.roundEach
                { topLeft = 15
                , bottomLeft = 15
                , topRight = 0
                , bottomRight = 0
                }
                :: Border.widthEach { bottom = 4, top = 4, left = 4, right = 2 }
                :: list

        STC.End c ->
            Border.roundEach
                { topLeft = 0
                , bottomLeft = 0
                , topRight = 15
                , bottomRight = 15
                }
                :: Border.widthEach { bottom = 4, top = 4, left = 2, right = 4 }
                :: list

        STC.Middle c ->
            Border.widthEach { bottom = 4, top = 4, left = 2, right = 2 }
                :: list


color : STC.Color -> Bool -> List (Element.Attribute msg) -> List (Element.Attribute msg)
color c on list =
    let
        x =
            if on then
                150

            else
                50
    in
    case c of
        STC.Yellow ->
            Background.color (Element.rgb255 x x 0) :: list

        STC.Red ->
            Background.color (Element.rgb255 x 0 0) :: list
